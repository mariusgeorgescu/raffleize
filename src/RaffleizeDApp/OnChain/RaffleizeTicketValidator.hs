{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module RaffleizeDApp.OnChain.RaffleizeTicketValidator where

import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V3
import PlutusTx
  ( CompiledCode,
    compile,
    liftCode,
    unsafeApplyCode,
  )
import RaffleizeDApp.Constants (secretMaxLength)
import RaffleizeDApp.CustomTypes.ActionTypes
  ( RaffleOwnerAction (GetCollateralOfExpiredTicket),
    RaffleizeRedeemer (RaffleOwnerRedeemer, TicketOwnerRedeemer),
    TicketOwnerAction (RefundCollateralLosing, RevealTicketSecret),
  )
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
  ( TicketDatum (..),
    TicketStateData (..),
    TicketStateId (..),
    mkTicketDatum,
  )
import RaffleizeDApp.OnChain.RaffleizeLogic
  ( deriveUserFromRefAC,
    evalTicketState,
    evaluateRaffleState,
    generateTicketACFromTicket,
    unsafeGetRaffleStateDatumAndValue,
    isOneOutputTo,
    revealTicketToRaffleT,
  )
import RaffleizeDApp.OnChain.Utils
  ( AScriptContext (AScriptContext),
    ATxInfo (..),
    findTxInWith,
    unsafeGetOwnInput,
    hasTxInWithToken,
    hasTxOutWithInlineDatumAnd,
    isBurningNFT,
    mkUntypedLambda,
    noConstraint,
    spendsToken,
  )

--- *  Validator Lambda

ticketValidatorLamba :: PubKeyHash -> AScriptContext -> Bool
ticketValidatorLamba adminPKH context@(AScriptContext ATxInfo {..} (Redeemer bredeemer) (SpendingScript _outRef (Just (Datum bdatum)))) =
  case (fromBuiltinData bdatum, fromBuiltinData bredeemer) of
    (Just (TicketDatum _ _ tsd@TicketStateData {..}), Just redeemer) ->
      let (!ticketRefAC, !ticketUserAC) = generateTicketACFromTicket tsd
       in if spendsToken ticketRefAC context --- Transaction spends the ticket ref NFT in the currently validating input.
            then
              let !raffleRefAC = tRaffle
                  !raffleValidatorAddr = scriptHashAddress tRaffleValidator
                  burnsTicketUserAndRef =
                    isBurningNFT ticketRefAC txInfoMint
                      && isBurningNFT ticketUserAC txInfoMint
               in case redeemer of
                    RaffleOwnerRedeemer GetCollateralOfExpiredTicket ->
                      let !raffleUserAC = deriveUserFromRefAC raffleRefAC
                          ---- RAFFLE STATE FROM REF INPUT
                          (!rValue, !rsd) = unsafeGetRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoReferenceInputs --- Transaction references the raffleRef.in ref inputs
                          !rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                          !currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                       in pand
                            [ currentTicketState #== UNREVEALED_EXPIRED, -- BURNABLE_BY_RAFFLE_OWNER (UNREVEALED_EXPIRED)
                              isBurningNFT ticketRefAC txInfoMint, -- Transaction burns 1 ticketRef.
                              hasTxInWithToken raffleUserAC txInfoInputs -- Transaction spends the raffle user NFT on another input.
                            ]
                    TicketOwnerRedeemer RefundCollateralLosing _ ->
                      let ---- RAFFLE STATE FROM REF INPUT
                          (!rValue, !rsd) = unsafeGetRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoReferenceInputs --- Transaction has the raffleRef as reference input
                          !rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                          !currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                       in pand
                            [ currentTicketState #== LOSING,
                              burnsTicketUserAndRef, -- Must burn tickets NFTs
                              traceIfFalse "are you stupid?" $
                                rRandomSeed rsd #/= tNumber -- Must not be the winning ticket
                            ]
                    TicketOwnerRedeemer !toa _ ->
                      -- rsd must be strict to ensure that raffle state is spent
                      let (rValue, rsd) = unsafeGetRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoInputs --- Must spend the raffleRef on another input.
                          rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                          currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                          ownInput = unsafeGetOwnInput context
                          ownValue = txOutValue ownInput
                          !ticketValidatorAddr = txOutAddress ownInput
                          hasOnly1InputFromValidator = case findTxInWith noConstraint (#== ticketValidatorAddr) txInfoInputs of
                            [_x] -> True
                            _ -> False
                       in ---- NO DOUBLE SATISFACTION when updating the raffle state
                          hasOnly1InputFromValidator -- Must be only one ticket action per transaction
                            && case toa of
                              RevealTicketSecret secret ->
                                let !new_tsd = revealTicketToRaffleT secret tsd
                                 in pand
                                      [ traceIfFalse "secret too long" $ lengthOfByteString secret #<= secretMaxLength,
                                        hasTxInWithToken ticketUserAC txInfoInputs, -- Transaction spends the ticket user NFT on another input.
                                        currentTicketState #== REVEALABLE, -- REVEALABLE -- Current state is valid for revealing ticket.
                                        hasTxOutWithInlineDatumAnd (mkTicketDatum new_tsd) (#== ownValue) (#== ticketValidatorAddr) txInfoOutputs ---Transaction locks new ticket state at validator address (with updated datum).
                                      ]
                              _otherClosingAction ->
                                -- burnsTicketUserAndRef -- Not checking the state to save memory; If you are stupid I let you be
                                pand
                                  [ currentTicketState `pelem` [FULLY_REFUNDABLE, EXTRA_REFUNDABLE, WINNING], -- BURNABLE_BY_TICKET_OWNER
                                    burnsTicketUserAndRef
                                  ]
                    _ -> traceIfFalse "Invalid redeemer for ticket ref" False
            else --- Transaction does not spend the ticket ref NFT in the currently validating input.
              isOneOutputTo txInfoOutputs adminPKH --- Must have exactly one outoput to admin
    _ -> False
ticketValidatorLamba _ _ = False

------------------------------------

-- | Untyped version of the spending validator lambda.
untypedTicketValidatorLamba :: PubKeyHash -> BuiltinData -> BuiltinUnit
untypedTicketValidatorLamba = mkUntypedLambda . ticketValidatorLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.

-- | Function for producing the compiled spending validator script.
compileTicketValidator :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinUnit)
compileTicketValidator pkh = $$(compile [||untypedTicketValidatorLamba||]) `unsafeApplyCode` liftCode plcVersion110 pkh

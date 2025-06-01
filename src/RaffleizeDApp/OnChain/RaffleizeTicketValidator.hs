{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module RaffleizeDApp.OnChain.RaffleizeTicketValidator where

import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V3
import PlutusTx
import RaffleizeDApp.CustomTypes.ActionTypes
  ( RaffleOwnerAction (GetCollateralOfExpiredTicket),
    RaffleizeRedeemer (RaffleOwnerRedeemer, TicketOwnerRedeemer),
    TicketOwnerAction (RefundCollateralLosing),
  )
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
  ( TicketDatum (..),
    TicketStateData (..),
    TicketStateId (..),
  )
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.OnChain.RaffleizeLogic
  ( checkTicketAction,
    deriveUserFromRefAC,
    evalTicketState,
    evaluateRaffleState,
    generateTicketACFromTicket,
    isValidTicketForRaffle,
    redeemerToAction,
    unsafeGetRaffleStateDatumAndValue,
  )
import RaffleizeDApp.OnChain.Utils
  ( hasTxInWithToken,
    isBurningNFT,
    mkUntypedLambda,
    ownInputHasToken,
  )

--- *  Validator Lambda

ticketValidatorLamba :: AScriptContext -> Bool
ticketValidatorLamba context@(AScriptContext ATxInfo {..} (Redeemer bredeemer) (SpendingScript _outRef (Just (Datum bdatum)))) =
  case (fromBuiltinData bdatum, fromBuiltinData bredeemer) of
    (Just (TicketDatum _ _ tsd@TicketStateData {..}), Just redeemer) ->
      let (!ticketRefAC, !ticketUserAC) = generateTicketACFromTicket tsd
       in --- Transaction spends the ticket ref NFT in the currently validating input.
          ( ownInputHasToken ticketRefAC context
              && ( let !raffleRefAC = tRaffle
                       !raffleValidatorAddr = scriptHashAddress tRaffleValidator
                       burnsTicketUserAndRef =
                         isBurningNFT ticketRefAC txInfoMint
                           && isBurningNFT ticketUserAC txInfoMint
                    in case redeemer of
                         RaffleOwnerRedeemer GetCollateralOfExpiredTicket ->
                           let !raffleUserAC = deriveUserFromRefAC raffleRefAC
                               ---- RAFFLE STATE FROM REF INPUT
                               -- Fails if the tx does not reference the corresponding raffle at the raffle validator address
                               (!rValue, !rsd) = unsafeGetRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoReferenceInputs
                               !rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                               !currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                            in pand
                                 [ currentTicketState #== UNREVEALED_EXPIRED, -- BURNABLE_BY_RAFFLE_OWNER (UNREVEALED_EXPIRED)
                                   isBurningNFT ticketRefAC txInfoMint, -- Transaction burns 1 ticketRef.
                                   hasTxInWithToken raffleUserAC txInfoInputs -- Transaction spends the raffle user NFT on another input.
                                 ]
                         TicketOwnerRedeemer RefundCollateralLosing _ ->
                           let ---- RAFFLE STATE FROM REF INPUT
                               -- Fails if the tx does not reference the corresponding raffle at the raffle validator address.
                               (!rValue, !rsd) = unsafeGetRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoReferenceInputs
                               !rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                               !currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                            in pand
                                 [ currentTicketState #== LOSING,
                                   burnsTicketUserAndRef -- Must burn both tickets NFTs
                                 ]
                         TicketOwnerRedeemer _action _ ->
                           let -- Fails if the tx does not spend the corresponding raffle at the raffle validator address, on another input.
                               !(rValue, rsd) = unsafeGetRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoInputs
                               rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                               currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                            in pand -- Validate raffle relationship (rest of validations are delegated to raffle validator)
                                 [ hasTxInWithToken ticketUserAC txInfoInputs, -- Must prove ownership of the user token
                                   isValidTicketForRaffle ticketUserAC tsd (rRaffleID rsd), -- The ticket must be of the current raffle
                                   checkTicketAction (redeemerToAction redeemer) currentTicketState -- Redeemer action must be valid for the current ticket state.
                                 ]
                         _ -> traceIfFalse "Invalid redeemer for ticket ref" False
                 )
          )
    (Just _, Nothing) -> traceError "invalid redeemer"
    (Nothing, _) -> trace "Output with invalid datum" True --  Free to go
ticketValidatorLamba _ = False

------------------------------------

-- | Untyped version of the spending validator lambda.
untypedTicketValidatorLamba :: BuiltinData -> BuiltinUnit
untypedTicketValidatorLamba = mkUntypedLambda ticketValidatorLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.

-- | Function for producing the compiled spending validator script.
compileTicketValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
compileTicketValidator = $$(compile [||untypedTicketValidatorLamba||])

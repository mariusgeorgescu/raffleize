{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}

module RaffleizeDApp.OnChain.RaffleizeTicketValidator where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V2 (PubKeyHash, TxOut (txOutAddress, txOutValue))
import PlutusTx (
  CompiledCode,
  compile,
  liftCode,
  unsafeApplyCode,
 )
import RaffleizeDApp.CustomTypes.ActionTypes (
  RaffleOwnerAction (GetCollateraOfExpiredTicket),
  RaffleizeRedeemer (RaffleOwnerRedeemer, TicketOwnerRedeemer),
  TicketOwnerAction (RefundCollateralLosing, RevealTicketSecret),
 )
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes (
  TicketDatum (..),
  TicketStateData (..),
  mkTicketDatum,
 )
import RaffleizeDApp.CustomTypes.Types (
  AScriptContext (AScriptContext),
  ATxInfo (..),
 )
import RaffleizeDApp.OnChain.RaffleizeLogic (
  deriveUserFromRefAC,
  evalTicketState,
  evaluateRaffleState,
  generateTicketACFromTicket,
  getRaffleStateDatumAndValue,
  isOneOutputTo,
  revealTicketToRaffleT,
 )
import RaffleizeDApp.OnChain.Utils (
  findTxInWith,
  getOwnInput,
  hasTxInWithToken,
  hasTxOutWith,
  hasTxOutWithInlineDatumAnd,
  isBurningNFT,
  mkUntypedValidatorCustom,
  noConstraint,
  spendsToken,
 )

--- *  Validator Lambda

ticketValidatorLamba :: PubKeyHash -> TicketDatum -> RaffleizeRedeemer -> AScriptContext -> Bool
ticketValidatorLamba adminPKH (TicketDatum _ _ tsd@TicketStateData {..}) redeemer context@(AScriptContext ATxInfo {..} _) =
  let
    (!ticketRefAC, !ticketUserAC) = generateTicketACFromTicket tsd
   in
    if spendsToken ticketRefAC context --- Transaction spends the ticket ref NFT in the currently validating input.
      then
        let
          !raffleRefAC = tRaffle
          raffleValidatorAddr = scriptHashAddress tRaffleValidator
          paysValueTo value addr = hasTxOutWith (`geq` value) (#== addr) txInfoOutputs
          burnsTicketUserAndRef =
            isBurningNFT ticketRefAC txInfoMint
              && isBurningNFT ticketUserAC txInfoMint
          !ownInput = getOwnInput context
          ownValue = txOutValue ownInput
         in
          case redeemer of
            RaffleOwnerRedeemer GetCollateraOfExpiredTicket ->
              let raffleUserAC = deriveUserFromRefAC raffleRefAC
                  ---- RAFFLE STATE FROM REF INPUT
                  (rValue, rsd) = getRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoReferenceInputs --- Transaction references the raffleRef.in ref inputs
                  rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                  currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
               in pand
                    [ currentTicketState #== 97 -- BURNABLE_BY_RAFFLE_OWNER (UNREVEALED_EXPIRED)
                    , isBurningNFT ticketRefAC txInfoMint -- Transaction burns 1 ticketRef.
                    , hasTxInWithToken raffleUserAC txInfoInputs -- Transaction spends the raffle user NFT on another input.
                    ]
            TicketOwnerRedeemer RefundCollateralLosing _ ->
              let
                ---- RAFFLE STATE FROM REF INPUT
                (rValue, !rsd) = getRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoReferenceInputs --- Transaction has the raffleRef as reference input
                rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
               in
                pand
                  [ currentTicketState #== 95 -- LOSING
                  , burnsTicketUserAndRef -- Must burn tickets NFTs
                  , traceIfFalse "are you stupid?" $
                      rRandomSeed rsd #== tNumber -- Must not be the winning ticket
                  ]
            TicketOwnerRedeemer toa _ ->
              -- rsd must be strict to ensure that raffle state is spent
              let (!rValue, !rsd) = getRaffleStateDatumAndValue raffleRefAC (#== raffleValidatorAddr) txInfoInputs --- Must spend the raffleRef on another input.
                  rStateId = evaluateRaffleState (txInfoValidRange, rsd, rValue)
                  currentTicketState = evalTicketState tsd (rRandomSeed rsd) rStateId
                  !ticketValidatorAddr = txOutAddress ownInput
                  hasOnly1InputFromValidator = case findTxInWith noConstraint (#== ticketValidatorAddr) txInfoInputs of
                    [_x] -> True
                    _ -> False
               in ---- NO DOUBLE SATISFACTION when updating the raffle state
                  hasOnly1InputFromValidator -- Must be only one ticket action per transaction
                    && case toa of
                      RevealTicketSecret secret ->
                        let new_tsd = revealTicketToRaffleT secret tsd
                         in pand
                              [ traceIfFalse "secret too long" $ lengthOfByteString secret #<= 32
                              , hasTxInWithToken ticketUserAC txInfoInputs -- Transaction spends the ticket user NFT on another input.
                              , currentTicketState #== 92 -- REVEALABLE -- Current state is valid for revealing ticket.
                              , hasTxOutWithInlineDatumAnd (mkTicketDatum new_tsd) (#== ownValue) (#== ticketValidatorAddr) txInfoOutputs ---Transaction locks new ticket state at validator address (with updated datum).
                              ]
                      _closingAction ->
                        pand
                          [ currentTicketState `pelem` [91, 96, 94] -- BURNABLE_BY_TICKET_OWNER (FULLY_REFUNDABLE, EXTRA_REFUNDABLE, WINNING)
                          , burnsTicketUserAndRef
                          ]
            _ -> traceIfFalse "Invalid redeemer for ticket ref" False
      else --- Transaction does not spend the ticket ref NFT in the currently validating input.
        isOneOutputTo txInfoOutputs adminPKH --- Must have exactly one outoput to admin

------------------------------------

-- | Untyped version of the spending validator lambda.
untypedTicketValidatorLamba :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTicketValidatorLamba = mkUntypedValidatorCustom . ticketValidatorLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.

-- | Function for producing the compiled spending validator script.
compileTicketValidator :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compileTicketValidator pkh = $$(compile [||untypedTicketValidatorLamba||]) `unsafeApplyCode` liftCode plcVersion100 pkh

-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module RaffleizeDApp.OnChain.RaffleizeValidator where

import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1 (assetClassValue, pubKeyHashAddress)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V2 (Redeemer (Redeemer))
import PlutusLedgerApi.V3 (Datum (Datum), PubKeyHash, TxOut (txOutAddress, txOutValue), fromBuiltinData)
import PlutusLedgerApi.V3.Contexts
import PlutusTx
  ( CompiledCode,
    compile,
    liftCode,
    unsafeApplyCode,
  )
import RaffleizeDApp.Constants (secretMaxLength)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
  ( RaffleDatum (RaffleDatum),
    RaffleParam (..),
    RaffleStateData (..),
    mkRaffleDatum,
  )
import RaffleizeDApp.CustomTypes.TicketTypes
  ( TicketStateData,
    mkTicketDatum,
  )
import RaffleizeDApp.OnChain.RaffleizeLogic
  ( checkRaffleAction,
    checkRaffleConfig,
    checkTicketAction,
    deriveUserFromRefAC,
    evalTicketState,
    evaluateRaffleState,
    getNextTicketToMintAssetClasses,
    isValidTicketForRaffle,
    redeemerToAction,
    revealTicketToRaffleRT,
    unsafeGetTicketStateDatumAndValue,
    updateRaffleStateValue,
  )
import RaffleizeDApp.OnChain.Utils
  ( AScriptContext (..),
    ATxInfo (..),
    hasTxInWithToken,
    hasTxOutWith,
    hasTxOutWithInlineDatumAnd,
    isBurningNFT,
    isMintingNFT,
    mkUntypedLambda,
    ownInputHasToken,
    unsafeGetOwnInput,
  )

--- *  Validator Lambda

raffleizeValidatorLamba :: PubKeyHash -> AScriptContext -> Bool
raffleizeValidatorLamba adminPKH context@(AScriptContext ATxInfo {..} (Redeemer bredeemer) (SpendingScript _ (Just (Datum bdatum)))) =
  -- adminPKH -- TODO - instead of adminPKH use a specific Admin NFT
  let !ownInput = unsafeGetOwnInput context -- safe in this care since is SpendingScript
      !ownValue = txOutValue ownInput
   in case (fromBuiltinData bdatum, fromBuiltinData bredeemer) of
        (Just (RaffleDatum _mdata _vs rsd@RaffleStateData {rRaffleID, rParam, rRandomSeed}), Just redeemer) ->
          ---  Tx must spend the raffle ref NFT in the currently validating input.
          ownInputHasToken rRaffleID context
            && ( let !action = redeemerToAction redeemer
                     !raffleValidatorAddr = txOutAddress ownInput
                     !ticketValidatorAddr = scriptHashAddress (rTicketValidatorHash rParam)
                     !currentRaffleStateId = evaluateRaffleState (txInfoValidRange, rsd, ownValue)
                     locksRaffleStateWithUpdatedDatumAndValue (newStateData :: RaffleStateData) =
                       traceIfFalse "raffle state not locked" $
                         hasTxOutWithInlineDatumAnd
                           (mkRaffleDatum newStateData)
                           (#== updateRaffleStateValue action rsd ownValue)
                           (#== raffleValidatorAddr)
                           txInfoOutputs -- using (==) to avoid utxo value ddos
                     locksTicketStateWithUpdatedDatumAndValue (newStateData :: TicketStateData) newValue =
                       traceIfFalse "ticket state not locked" $
                         hasTxOutWithInlineDatumAnd
                           (mkTicketDatum newStateData)
                           (#== newValue)
                           (#== ticketValidatorAddr)
                           txInfoOutputs
                  in checkRaffleAction action currentRaffleStateId -- Reedeemer action must be valid for the current raffle state.
                       && case redeemer of
                         UserRedeemer _buy ->
                           -- can only be Buy action at this moment
                           let (!ticketRefAC, !ticketUserAC) = getNextTicketToMintAssetClasses rsd -- Generate tickets  based on no. of tickets sold.
                            in pand
                                 -- Check only if is minting correct NFTs
                                 -- Minting Policy checks that when minting tickets raffle state is updated and ticket state is created.
                                 [ isMintingNFT ticketRefAC txInfoMint, --  Must mint the ticket ref NFT.
                                   isMintingNFT ticketUserAC txInfoMint -- Must mint the ticket user NFT.
                                 ]
                         RaffleOwnerRedeemer raffleOwnerAction ->
                           let !raffleUserAC = deriveUserFromRefAC rRaffleID
                               !burnsRaffleUser = isBurningNFT raffleUserAC txInfoMint
                               !burnsRaffleRef = isBurningNFT rRaffleID txInfoMint
                            in traceIfFalse "Tx must spend raffle owner user NFT" $
                                 hasTxInWithToken raffleUserAC txInfoInputs -- Prove Raffle Ownership
                                   && case raffleOwnerAction of
                                     GetCollateralOfExpiredTicket -> traceError "invalid redeemer"
                                     Cancel ->
                                       pand
                                         [ burnsRaffleUser, -- Must burn raffle user NFT.
                                           burnsRaffleRef --  Must burn raffle ref NFT.
                                         ]
                                     Update newConfig ->
                                       pand
                                         [ traceIfFalse "The new raffle configuration must be valid" $
                                             checkRaffleConfig rParam txInfoValidRange newConfig,
                                           locksRaffleStateWithUpdatedDatumAndValue rsd {rConfig = newConfig} -- Must lock raffle state with value and data
                                         ]
                                     _raffleOwnerClosingAction ->
                                       pand
                                         [ burnsRaffleUser, -- Must burn raffle user NFT.
                                           locksRaffleStateWithUpdatedDatumAndValue rsd -- Must lock raffle state a with updated value. Data is not changing.
                                         ]
                         TicketOwnerRedeemer ticketOwnerAction !receivedAC ->
                           let -- Fails if the tx does not spend the NFT received in redeemer from the ticket validator in another input.
                               !(tValue, tsd) = unsafeGetTicketStateDatumAndValue receivedAC (#== ticketValidatorAddr) txInfoInputs
                               !userAC = deriveUserFromRefAC receivedAC
                               currentTicketState = evalTicketState tsd rRandomSeed currentRaffleStateId
                               isBurningTickets = isBurningNFT receivedAC txInfoMint && isBurningNFT userAC txInfoMint
                            in pand
                                 [ hasTxInWithToken userAC txInfoInputs, -- Must prove ownership of the user token
                                   isValidTicketForRaffle receivedAC tsd rRaffleID, -- The ticket must be of the current raffle
                                   checkTicketAction action currentTicketState -- Redeemer action must be valid for the current ticket state.
                                 ]
                                 && case ticketOwnerAction of
                                   RefundCollateralLosing -> traceError "invalid redeemer"
                                   RevealTicketSecret secret ->
                                     let (new_rsd, new_tsd) = revealTicketToRaffleRT secret tsd rsd
                                      in pand
                                           [ traceIfFalse "secret too long" $ lengthOfByteString secret #<= secretMaxLength,
                                             locksTicketStateWithUpdatedDatumAndValue new_tsd tValue, -- Must lock ticket state at Ticket Validator address (with updated datum)
                                             locksRaffleStateWithUpdatedDatumAndValue new_rsd -- Must lock raffle state at Raffle Validator address (with updated datum).
                                           ]
                                   CollectStake ->
                                     pand
                                       [ isBurningTickets,
                                         locksRaffleStateWithUpdatedDatumAndValue rsd
                                         -- Transaction locks new raffle state at validator address (with updated value).
                                       ]
                                   _refundAction ->
                                     pand
                                       [ isBurningTickets,
                                         locksRaffleStateWithUpdatedDatumAndValue rsd {rRefundedTickets = rRefundedTickets rsd #+ 1}
                                         --   Transaction locks new raffle state at validator address (with updated value and datum).
                                       ]
                         AdminRedeemer CloseRaffle ->
                           pand
                             [ --- Must burn the raffle ref
                               isBurningNFT rRaffleID txInfoMint,
                               --- Must pay value  to admin
                               hasTxOutWith (#== (ownValue #- assetClassValue rRaffleID 1)) (#== pubKeyHashAddress adminPKH) txInfoOutputs
                             ]
               )
        (Just _, Nothing) -> traceError "invalid redeemer"
        (Nothing, _) ->
          trace "Output with invalid datum" $ --  Spendable to admin only
            hasTxOutWith (#== ownValue) (#== pubKeyHashAddress adminPKH) txInfoOutputs
raffleizeValidatorLamba _ _ = False
{-# INLINEABLE raffleizeValidatorLamba #-}

------------------------------------

-- | Untyped version of the spending validator lambda.
untypedRaffleizeValidatorLamba :: PubKeyHash -> BuiltinData -> BuiltinUnit
untypedRaffleizeValidatorLamba = mkUntypedLambda . raffleizeValidatorLamba

-- 3. Pre-compilation

-- | Function for producing the compiled spending validator script.
compileRaffleizeValidator :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinUnit)
compileRaffleizeValidator params = $$(compile [||untypedRaffleizeValidatorLamba||]) `unsafeApplyCode` liftCode plcVersion110 params

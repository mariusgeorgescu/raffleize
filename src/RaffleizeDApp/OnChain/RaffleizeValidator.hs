-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module RaffleizeDApp.OnChain.RaffleizeValidator where

import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (assetClassValue)
import PlutusLedgerApi.V2 (Redeemer (Redeemer))
import PlutusLedgerApi.V3 (Datum (Datum), PubKeyHash, TxOut (txOutAddress, txOutValue), fromBuiltinData)
import PlutusLedgerApi.V3.Contexts
import PlutusTx
  ( CompiledCode,
    compile,
    liftCode,
    unsafeApplyCode,
  )
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
  ( RaffleDatum (RaffleDatum),
    RaffleParam (..),
    RaffleStateData (..),
    mkRaffleDatum,
  )
import RaffleizeDApp.CustomTypes.TicketTypes
  ( TicketStateData (tNumber),
    mkTicketDatum,
  )
import RaffleizeDApp.OnChain.RaffleizeLogic
  ( buyTicketToRaffle,
    checkRaffleAction,
    checkRaffleConfig,
    deriveUserFromRefAC,
    evaluateRaffleState,
    getNextTicketToMintAssetClasses,
    isOneOutputTo,
    isTicketForRaffle,
    redeemerToAction,
    refundTicketToRaffle,
    revealTicketToRaffleR,
    ticketCollateralValue,
    unsafeGetTicketStateDatumAndValue,
    updateRaffleStateValue,
  )
import RaffleizeDApp.OnChain.Utils
  ( AScriptContext (..),
    ATxInfo (..),
    hasTxInWithToken,
    hasTxOutWithInlineDatumAnd,
    isBurningNFT,
    isMintingNFT,
    mkUntypedLambda,
    spendsToken,
    unsafeGetOwnInput,
  )

--- *  Validator Lambda

raffleizeValidatorLamba :: PubKeyHash -> AScriptContext -> Bool
raffleizeValidatorLamba adminPKH context@(AScriptContext ATxInfo {..} (Redeemer bredeemer) (SpendingScript _ (Just (Datum bdatum)))) =
  -- adminPKH -- TODO - instead of adminPKH use a specific Admin NFT
  case (fromBuiltinData bdatum, fromBuiltinData bredeemer) of
    (Just (RaffleDatum _mdata _vs rsd@RaffleStateData {rRaffleID, rParam, rRandomSeed}), Just redeemer) ->
      if spendsToken rRaffleID context --- Must spend the raffle ref NFT in the currently validating input.
        then
          let !ownInput = unsafeGetOwnInput context -- Fails if does
              !ownValue = txOutValue ownInput
              !action = redeemerToAction redeemer
              !updatedRaffleStateValue = updateRaffleStateValue action rsd ownValue
              !raffleValidatorAddr = txOutAddress ownInput
              !ticketValidatorAddr = scriptHashAddress (rTicketValidatorHash rParam)
              !currentStateLabel = evaluateRaffleState (txInfoValidRange, rsd, ownValue)
              locksRaffleStateWithUpdatedDatumAndValue (newStateData :: RaffleStateData) =
                traceIfFalse "raffle state not locked" $
                  hasTxOutWithInlineDatumAnd (mkRaffleDatum newStateData) (#== updatedRaffleStateValue) (#== raffleValidatorAddr) txInfoOutputs -- using (==) to avoid utxo value ddos
              hasNewTicketState (newStateData :: TicketStateData) newValue =
                traceIfFalse "ticket state not locked" $
                  hasTxOutWithInlineDatumAnd (mkTicketDatum newStateData) newValue (#== ticketValidatorAddr) txInfoOutputs
           in checkRaffleAction action currentStateLabel -- Must be a valid action for the raffle state.
                && case redeemer of
                  UserRedeemer (CreateRaffle _) -> traceIfFalse "invalid redeemer" False
                  UserRedeemer (BuyTicket secret) ->
                    let (!ticketRefAC, !ticketUserAC) = getNextTicketToMintAssetClasses rsd -- Generate tickets  based on no. of tickets sold.
                        !ticketRefNFT = assetClassValue ticketRefAC 1
                        (!new_rsd, !new_tsd) = buyTicketToRaffle secret rsd (rRaffleValidatorHash rParam)
                     in pand
                          [ isMintingNFT ticketRefAC txInfoMint, --  Must mint the ticket ref NFT.
                            isMintingNFT ticketUserAC txInfoMint, -- Must mint the ticket user NFT.
                            locksRaffleStateWithUpdatedDatumAndValue new_rsd, -- Must lock raffle state at Raffle Validator address (with updated value and datum).
                            hasNewTicketState new_tsd (#== (ticketRefNFT #+ ticketCollateralValue rsd)) -- Must lock ticket state at Ticket Validator address (with collateral value and valid datum).
                          ]
                  RaffleOwnerRedeemer raffleOwnerAction ->
                    let !raffleUserAC = deriveUserFromRefAC rRaffleID
                        !burnsRaffleUser = isBurningNFT raffleUserAC txInfoMint
                        !burnsRaffleRef = isBurningNFT rRaffleID txInfoMint
                     in case raffleOwnerAction of
                          GetCollateralOfExpiredTicket -> traceError "never"
                          Cancel ->
                            pand
                              [ burnsRaffleUser, -- Must burn raffle user NFT.
                                burnsRaffleRef --  Must burn raffle ref NFT.
                              ]
                          Update newConfig ->
                            pand
                              [ checkRaffleConfig rParam txInfoValidRange newConfig, -- Must be a valid configuration.
                                locksRaffleStateWithUpdatedDatumAndValue rsd {rConfig = newConfig} -- Must lock raffle state with updated config.
                              ]
                          _raffleOwnerClosingAction ->
                            pand
                              [ burnsRaffleUser, -- Must burn raffle user NFT.
                                locksRaffleStateWithUpdatedDatumAndValue rsd -- Must lock raffle state at Raffle Validator address (with updated value and datum).
                              ]
                  TicketOwnerRedeemer !ticketOwnerAction !ticketRefAC ->
                    let !tsd = snd $ unsafeGetTicketStateDatumAndValue ticketRefAC (#== ticketValidatorAddr) txInfoInputs --- Must spend the ticket ref NFT in another input.
                        !ticketUserAC = deriveUserFromRefAC ticketRefAC
                        burnsTickets = isBurningNFT ticketRefAC txInfoMint && isBurningNFT ticketUserAC txInfoMint
                     in hasTxInWithToken ticketUserAC txInfoInputs -- Must spend the ticket user NFT
                          && case ticketOwnerAction of
                            RefundCollateralLosing -> traceError "never"
                            RevealTicketSecret secret ->
                              pand
                                [ -- , hasNewTicketState new_tsd tValue -- Must lock ticket state at Ticket Validator address (with updated datum)
                                  locksRaffleStateWithUpdatedDatumAndValue (revealTicketToRaffleR secret tsd rsd), -- Must lock raffle state at Raffle Validator address (with updated datum).
                                  isTicketForRaffle ticketRefAC tsd rsd
                                ]
                            CollectStake ->
                              pand
                                [ traceIfFalse "not winning ticket  " $ rRandomSeed #== tNumber tsd, -- check if the current ticket is the winning ticket
                                  burnsTickets,
                                  locksRaffleStateWithUpdatedDatumAndValue rsd -- Transaction locks new raffle state at validator address (with updated value).
                                ]
                            _refundAction ->
                              pand
                                [ burnsTickets,
                                  locksRaffleStateWithUpdatedDatumAndValue (refundTicketToRaffle tsd rsd) -- Transaction locks new raffle state at validator address (with updated value and datum).
                                ]
                  AdminRedeemer CloseRaffle ->
                    let raffleUserAC = deriveUserFromRefAC rRaffleID
                        burnsRaffleUser = isBurningNFT raffleUserAC txInfoMint
                        burnsRaffleRef = isBurningNFT rRaffleID txInfoMint
                     in pand
                          [ burnsRaffleRef,
                            burnsRaffleUser,
                            isOneOutputTo txInfoOutputs adminPKH --- Must have exactly one outoput to admin
                          ]
        else --- Transaction does not spend the raffle ref NFT in the currently validating input.
          isOneOutputTo txInfoOutputs adminPKH --- Must have exactly one outoput to
    _ -> False
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

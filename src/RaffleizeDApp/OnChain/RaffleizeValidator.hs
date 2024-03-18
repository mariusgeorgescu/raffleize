-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}

module RaffleizeDApp.OnChain.RaffleizeValidator where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (assetClassValue)
import PlutusLedgerApi.V2 (
  PubKeyHash,
  TxOut (txOutAddress, txOutValue),
 )
import PlutusTx (
  CompiledCode,
  compile,
  liftCode,
  unsafeApplyCode,
 )
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes (
  RaffleDatum (RaffleDatum),
  RaffleParam (..),
  RaffleStateData (..),
  mkRaffleDatum,
 )
import RaffleizeDApp.CustomTypes.TicketTypes (
  TicketStateData (tNumber),
  mkTicketDatum,
 )
import RaffleizeDApp.CustomTypes.Types (
  AScriptContext (AScriptContext),
  ATxInfo (
    ATxInfo,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoValidRange
  ),
 )
import RaffleizeDApp.OnChain.RaffleizeLogic (
  buyTicketToRaffle,
  checkRaffle,
  deriveUserFromRefAC,
  evaluateRaffleState,
  getNextTicketToMintAssetClasses,
  getTicketStateDatumAndValue,
  isOneOutputTo,
  isTicketForRaffle,
  raffleTicketCollateralValue,
  refundTicketToRaffle,
  revealTicketToRaffleR,
  updateRaffleStateValue,
  validateRaffleAction, redeemerToAction,
 )
import RaffleizeDApp.OnChain.Utils (
  getOwnInput,
  hasTxInWithToken,
  hasTxOutWithInlineDatumAnd,
  isBurningNFT,
  isMintingNFT,
  mkUntypedValidatorCustom,
  spendsToken,
 )

--- *  Validator Lambda

raffleizeValidatorLamba :: PubKeyHash -> RaffleDatum -> RaffleizeRedeemer -> AScriptContext -> Bool
raffleizeValidatorLamba
  adminPKH -- TODO - instead of adminPKH use a specific Admin NFT
  (RaffleDatum _mdata _vs rsd@RaffleStateData {rRaffleID, rParam, rRandomSeed})
  redeemer
  context@(AScriptContext ATxInfo {txInfoValidRange, txInfoMint, txInfoInputs, txInfoOutputs} _scrpurp) =
    if spendsToken rRaffleID context --- Must spend the raffle ref NFT in the currently validating input.
      then
        let ownInput = getOwnInput context
            !ownValue = txOutValue ownInput
            !action = redeemerToAction redeemer
            !updatedRaffleStateVale = updateRaffleStateValue action rsd ownValue
            !raffleValidatorAddr = txOutAddress ownInput
            !ticketValidatorAddr = scriptHashAddress (rTicketValidatorHash rParam)
            !currentStateLabel = evaluateRaffleState (txInfoValidRange, rsd, ownValue)
            raffleUserAC = deriveUserFromRefAC rRaffleID
            burnsRaffleRef = isBurningNFT rRaffleID txInfoMint
            locksRaffleStateWithUpdatedDatumAndValue (newStateData :: RaffleStateData) =
              traceIfFalse "raffle state not locked" $
                hasTxOutWithInlineDatumAnd (mkRaffleDatum newStateData) (#== updatedRaffleStateVale) (#== raffleValidatorAddr) txInfoOutputs -- using (==) to avoid utxo value ddos
            hasNewTicketState (newStateData :: TicketStateData) newValue =
              traceIfFalse "ticket state not locked" $
                hasTxOutWithInlineDatumAnd (mkTicketDatum newStateData) (#== newValue) (#== ticketValidatorAddr) txInfoOutputs
         in validateRaffleAction action currentStateLabel -- Must be a valid action for the raffle state.
              && case redeemer of
                UserRedeemer (CreateRaffle _) -> traceIfFalse "invalid redeemer" False
                UserRedeemer (BuyTicket secret) ->
                  let (ticketRefAC, ticketUserAC) = getNextTicketToMintAssetClasses rsd -- Generate tickets  based on no. of tickets sold.
                      ticketRefNFT = assetClassValue ticketRefAC 1
                      (new_rsd, new_tsd) = buyTicketToRaffle secret rsd (rRaffleValidatorHash rParam)
                   in pand
                        [ isMintingNFT ticketRefAC txInfoMint --  Must mint the ticket ref NFT.
                        , isMintingNFT ticketUserAC txInfoMint -- Must mint the ticket user NFT.
                        , locksRaffleStateWithUpdatedDatumAndValue new_rsd -- Must lock raffle state at Raffle Validator address (with updated value and datum).
                        , hasNewTicketState new_tsd (ticketRefNFT #+ raffleTicketCollateralValue rsd) -- Must lock ticket state at Ticket Validator address (with collateral value and valid datum).
                        ]
                RaffleOwnerRedeemer raffleOwnerAction ->
                  let
                    !burnsRaffleUser = isBurningNFT raffleUserAC txInfoMint
                   in
                    case raffleOwnerAction of
                      GetCollateraOfExpiredTicket -> traceError "never"
                      Cancel ->
                        pand
                          [ burnsRaffleUser -- Must burn raffle user NFT.
                          , burnsRaffleRef --  Must burn raffle ref NFT.
                          ]
                      Update newConfig ->
                        pand
                          [ checkRaffle rParam txInfoValidRange newConfig -- Must be a valid configuration.
                          , locksRaffleStateWithUpdatedDatumAndValue rsd {rConfig = newConfig} -- Must lock raffle state with updated config.
                          ]
                      _raffleOwnerClosingAction ->
                        pand
                          [ burnsRaffleUser -- Must burn raffle user NFT.
                          , locksRaffleStateWithUpdatedDatumAndValue rsd -- Must lock raffle state at Raffle Validator address (with updated value and datum).
                          ]
                TicketOwnerRedeemer ticketOwnerAction !ticketRefAC ->
                  let !tsd = snd $ getTicketStateDatumAndValue ticketRefAC (#== ticketValidatorAddr) txInfoInputs --- Must spend the ticket ref NFT in another input.
                      !ticketUserAC = deriveUserFromRefAC ticketRefAC
                      burnsTickets = isBurningNFT ticketRefAC txInfoMint && isBurningNFT ticketUserAC txInfoMint
                   in hasTxInWithToken ticketUserAC txInfoInputs -- Must spend the ticket user NFT
                        && case ticketOwnerAction of
                          RefundCollateralLosing -> traceError "never"
                          RevealTicketSecret secret ->
                            pand
                              [ -- , hasNewTicketState new_tsd tValue -- Must lock ticket state at Ticket Validator address (with updated datum)
                                locksRaffleStateWithUpdatedDatumAndValue (revealTicketToRaffleR secret tsd rsd) -- Must lock raffle state at Raffle Validator address (with updated datum).
                              , isTicketForRaffle ticketRefAC tsd rsd
                              ]
                          CollectStake ->
                            pand
                              [ traceIfFalse "not winning ticket  " $ rRandomSeed #== tNumber tsd -- check if the current ticket is the winning ticket
                              , burnsTickets
                              , locksRaffleStateWithUpdatedDatumAndValue rsd -- Transaction locks new raffle state at validator address (with updated value).
                              ]
                          _refundAction ->
                            pand
                              [ burnsTickets
                              , locksRaffleStateWithUpdatedDatumAndValue (refundTicketToRaffle tsd rsd) -- Transaction locks new raffle state at validator address (with updated value and datum).
                              ]
                AdminRedeemer CloseRaffle ->
                  let burnsRaffleUser = isBurningNFT raffleUserAC txInfoMint
                   in pand
                        [ burnsRaffleRef
                        , burnsRaffleUser
                        , isOneOutputTo txInfoOutputs adminPKH --- Must have exactly one outoput to admin
                        ]
      else --- Transaction does not spend the raffle ref NFT in the currently validating input.
        isOneOutputTo txInfoOutputs adminPKH --- Must have exactly one outoput to admin
{-# INLINEABLE raffleizeValidatorLamba #-}

------------------------------------

-- | Untyped version of the spending validator lambda.
untypedRaffleizeValidatorLamba :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedRaffleizeValidatorLamba = mkUntypedValidatorCustom . raffleizeValidatorLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.

-- | Function for producing the compiled spending validator script.
compileRaffleizeValidator :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compileRaffleizeValidator params = $$(compile [||untypedRaffleizeValidatorLamba||]) `unsafeApplyCode` liftCode plcVersion100 params

------------------------
------------------------
------------------------
------------------------
------------------------

-- sampleAdminAddr :: PubKeyHash
-- sampleAdminAddr = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"

-- sampleRaffleizeValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- sampleRaffleizeValidator = compileRaffleizeValidator sampleAdminAddr

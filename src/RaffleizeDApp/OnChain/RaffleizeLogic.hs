module RaffleizeDApp.OnChain.RaffleizeLogic where

import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (after, before)
import PlutusLedgerApi.V1.Value (AssetClass (..), adaSymbol, adaToken, assetClass, assetClassValueOf, geq, valueOf)
import PlutusLedgerApi.V3
  ( POSIXTime (POSIXTime),
    POSIXTimeRange,
    PubKeyHash,
    ScriptHash,
    ToData (toBuiltinData),
    TokenName (TokenName),
    TxInInfo,
    TxOut (..),
    UnsafeFromData (unsafeFromBuiltinData),
    Value, Address,
  )
import PlutusTx.Builtins
  ( divideInteger,
    modInteger,
    serialiseData,
  )
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
  ( RaffleConfig (..),
    RaffleParam (..),
    RaffleStateData (..),
    RaffleStateId (..),
    raffleStateData,
  )
import RaffleizeDApp.CustomTypes.TicketTypes (SecretHash, TicketStateData (..), TicketStateId (..), ticketStateData)
import RaffleizeDApp.OnChain.Utils (AddressConstraint, adaValueFromLovelaces, bsToInteger', integerToBs24, isTxOutWith, noConstraint, unsafeGetCurrentStateDatumAndValue)
import Prelude

------------------------------------------------------------------------------------------------

-- * RaffleStateData Helpers

------------------------------------------------------------------------------------------------
redeemerToAction :: RaffleizeRedeemer -> RaffleizeAction
redeemerToAction (UserRedeemer action) = User action
redeemerToAction (RaffleOwnerRedeemer action) = RaffleOwner action
redeemerToAction (TicketOwnerRedeemer action _) = TicketOwner action
redeemerToAction (AdminRedeemer action) = Admin action
{-# INLINEABLE redeemerToAction #-}

------------------------------------------------------------------------------------------------

-- * RaffleStateData Helpers

------------------------------------------------------------------------------------------------
raffleTicketPriceValue :: RaffleStateData -> PlutusLedgerApi.V3.Value
raffleTicketPriceValue RaffleStateData {rConfig} = adaValueFromLovelaces (rTicketPrice rConfig)
{-# INLINEABLE raffleTicketPriceValue #-}

raffleAccumulatedValue :: RaffleStateData -> PlutusLedgerApi.V3.Value
raffleAccumulatedValue RaffleStateData {rConfig, rSoldTickets} = adaValueFromLovelaces $ rTicketPrice rConfig #* rSoldTickets
{-# INLINEABLE raffleAccumulatedValue #-}

raffleCollateralValue :: RaffleStateData -> PlutusLedgerApi.V3.Value
raffleCollateralValue RaffleStateData {rParam} = adaValueFromLovelaces $ rRaffleCollateral rParam
{-# INLINEABLE raffleCollateralValue #-}

ticketCollateralValue :: RaffleStateData -> PlutusLedgerApi.V3.Value
ticketCollateralValue RaffleStateData {rParam} = adaValueFromLovelaces $ rTicketCollateral rParam
{-# INLINEABLE ticketCollateralValue #-}

------------------------------------------------------------------------------------------------

-- * Validation Logic

------------------------------------------------------------------------------------------------

-- | Check Parameters Validity
checkRaffleParam :: RaffleParam -> Bool
checkRaffleParam RaffleParam {..} =
  pand
    [ rMinRevealingWindow #> 0,
      rMaxNoOfTickets #> 0,
      rTicketCollateral #>= 2_000_000,
      rRaffleCollateral #>= 2_000_000
    ]
{-# INLINEABLE checkRaffleParam #-}

-- | Check Raffle Configuration Validity
checkRaffleConfig :: RaffleParam -> PlutusLedgerApi.V3.POSIXTimeRange -> RaffleConfig -> Bool
checkRaffleConfig
  param@RaffleParam {rMinRevealingWindow, rMinTicketPrice}
  timeRange
  RaffleConfig {rRevealDDL, rCommitDDL, rTicketPrice, rMinTickets, rStake} =
    pand
      [ checkRaffleParam param,
        traceIfFalse "invalid commit ddl" $
          rCommitDDL `after` timeRange,
        traceIfFalse "invalid reveal ddl" $
          rRevealDDL #>= rCommitDDL #+ PlutusLedgerApi.V3.POSIXTime rMinRevealingWindow,
        traceIfFalse "invalid ticket price" $
          rTicketPrice #>= rMinTicketPrice,
        traceIfFalse "invalid min tickets" $
          rMinTickets #> 0,
        traceIfFalse "empty stake" $
          rStake #/= mempty,
        traceIfFalse "stake should not contain ADA" $ -- to avoid double satisfaction when checking if stake is locked. -- to avoid double satisfaction when checking if stake is locked.
           -- to avoid double satisfaction when checking if stake is locked.
          assetClassValueOf rStake (assetClass adaSymbol adaToken) #== 0
      ]
{-# INLINEABLE checkRaffleConfig #-}

-- | Check if an action is valid on a state.
checkRaffleAction :: RaffleizeAction -> RaffleStateId -> Bool
checkRaffleAction action currentStateLabel =
  traceIfFalse "Action not permited in this raffle state" $
    currentStateLabel `pelem` validRaffleStatesForRaffleizeAction action
  where
    validRaffleStatesForRaffleizeAction :: RaffleizeAction -> [RaffleStateId]
    validRaffleStatesForRaffleizeAction = \case
      User (CreateRaffle _) -> []
      User (BuyTicket _) ->
        [ NEW,
          COMMITTING
        ]
      RaffleOwner roa -> case roa of
        Cancel -> [NEW]
        (Update _) -> [NEW]
        RecoverStake ->
          [ EXPIRED_LOCKED_STAKE,
            UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS,
            UNDERFUNDED_LOCKED_STAKE,
            UNREVEALED_LOCKED_STAKE_AND_REFUNDS,
            UNREVEALED_LOCKED_STAKE
          ]
        RecoverStakeAndAmount -> [UNREVEALED_NO_REVEALS]
        CollectAmount ->
          [ SUCCESS_LOCKED_STAKE_AND_AMOUNT,
            SUCCESS_LOCKED_AMOUNT
          ]
        GetCollateralOfExpiredTicket -> [] -- ticket action, not on raffle state
      TicketOwner toa -> case toa of
        (RevealTicketSecret _) -> [REVEALING]
        CollectStake ->
          [ SUCCESS_LOCKED_STAKE_AND_AMOUNT,
            SUCCESS_LOCKED_STAKE
          ]
        RefundTicket ->
          [ UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS,
            UNDERFUNDED_LOCKED_REFUNDS
          ]
        RefundTicketExtra ->
          [ UNREVEALED_LOCKED_STAKE_AND_REFUNDS,
            UNREVEALED_LOCKED_REFUNDS
          ]
        RefundCollateralLosing -> [] -- ticket action, not on raffle state
      Admin _ ->
        [ EXPIRED_FINAL,
          UNDERFUNDED_FINAL,
          UNREVEALED_FINAL,
          SUCCESS_FINAL
        ]
{-# INLINEABLE checkRaffleAction #-}

-- | Function that returns the updated value of a raffle.
-- Receives  current raffle state (and configuration), current value locked and and an action.
-- Returns the updated value after the action.
updateRaffleStateValue :: RaffleizeAction -> RaffleStateData -> Value -> Value
updateRaffleStateValue action rsd@RaffleStateData {rConfig, rSoldTickets, rRevealedTickets} currentValue = case action of
  User (BuyTicket _) -> currentValue #+ raffleTicketPriceValue rsd
  RaffleOwner RecoverStake -> currentValue #- rStake rConfig
  RaffleOwner RecoverStakeAndAmount -> currentValue #- rStake rConfig #- raffleAccumulatedValue rsd
  RaffleOwner CollectAmount -> currentValue #- raffleAccumulatedValue rsd
  RaffleOwner (Update newConfig) -> (currentValue #- rStake rConfig) #+ rStake newConfig
  TicketOwner (RevealTicketSecret _) -> currentValue
  TicketOwner CollectStake -> currentValue #- rStake rConfig
  TicketOwner RefundTicket -> currentValue #- raffleTicketPriceValue rsd
  TicketOwner RefundTicketExtra ->
    let extraRefundValue =
          adaValueFromLovelaces (rSoldTickets #* rTicketPrice rConfig `divideInteger` rRevealedTickets)
     in currentValue #- extraRefundValue
  Admin CloseRaffle -> trace "no raffle state should exist after this action" pmempty
  RaffleOwner Cancel -> trace "no raffle state should exist after this action" pmempty
  _ -> traceError "Invalid action on existing raffle"
{-# INLINEABLE updateRaffleStateValue #-}

evaluateRaffleState :: (PlutusLedgerApi.V3.POSIXTimeRange, RaffleStateData, PlutusLedgerApi.V3.Value) -> RaffleStateId
evaluateRaffleState (time_range, RaffleStateData {rParam, rConfig, rSoldTickets, rRevealedTickets, rRefundedTickets}, svalue) =
  let isBeforeCommitDDL = after (rCommitDDL rConfig) time_range
      isBetweenCommitAndRevealDDL = before (rCommitDDL rConfig) time_range && after (rRevealDDL rConfig) time_range
      isStakeLocked = svalue `geq` rStake rConfig
      isCollectedAmmoutLocked = (valueOf svalue adaSymbol adaToken #- rRaffleCollateral rParam) #>= (rTicketPrice rConfig #* rSoldTickets) -- lovelaces wo collateral >  sold ticket * ticket price
      outstandingFullRefunds = rRefundedTickets #< rSoldTickets
      outstandingExtraRefunds = rRefundedTickets #< rRevealedTickets
      anyTicketsSold = rSoldTickets #> 0
      minNoOfTicketsSold = rSoldTickets #< rMinTickets rConfig
      anyTicketsRevealed = rRevealedTickets #> 0
      allTicketsRevealed = rSoldTickets #== rRevealedTickets
   in if isBeforeCommitDDL
        then
          if anyTicketsSold
            then COMMITTING
            else NEW
        else
          if not anyTicketsSold
            then
              if isStakeLocked
                then EXPIRED_LOCKED_STAKE
                else EXPIRED_FINAL
            else
              if minNoOfTicketsSold
                then -- UNDERFUNDED
                  case (isStakeLocked, outstandingFullRefunds) of
                    (True, True) -> UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS
                    (False, True) -> UNDERFUNDED_LOCKED_REFUNDS
                    (True, False) -> UNDERFUNDED_LOCKED_STAKE
                    (False, False) -> UNDERFUNDED_FINAL
                else
                  if allTicketsRevealed
                    then -- SUCCESS
                      case (isStakeLocked, isCollectedAmmoutLocked) of
                        (True, True) -> SUCCESS_LOCKED_STAKE_AND_AMOUNT
                        (False, True) -> SUCCESS_LOCKED_AMOUNT
                        (True, False) -> SUCCESS_LOCKED_STAKE
                        (False, False) -> SUCCESS_FINAL
                    else
                      if isBetweenCommitAndRevealDDL
                        then REVEALING
                        else case (anyTicketsRevealed, isStakeLocked, outstandingExtraRefunds) of
                          (False, True, False) -> UNREVEALED_NO_REVEALS
                          (True, True, True) -> UNREVEALED_LOCKED_STAKE_AND_REFUNDS
                          (True, False, True) -> UNREVEALED_LOCKED_REFUNDS
                          (True, True, False) -> UNREVEALED_LOCKED_STAKE
                          (_, False, False) -> UNREVEALED_FINAL
                          (False, _, True) -> traceError "no refunds when 0 tickets are revealed"
{-# INLINEABLE evaluateRaffleState #-}

evalTicketState :: TicketStateData -> Integer -> RaffleStateId -> TicketStateId
evalTicketState TicketStateData {tNumber, tSecret} randomSeed raffleStateId =
  case raffleStateId of
    COMMITTING -> COMMITTED
    UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS -> FULLY_REFUNDABLE
    UNDERFUNDED_LOCKED_REFUNDS -> FULLY_REFUNDABLE
    REVEALING -> if isNothing tSecret then REVEALABLE else REVEALED
    SUCCESS_LOCKED_STAKE_AND_AMOUNT -> if randomSeed #== tNumber then WINNING else LOSING
    SUCCESS_LOCKED_STAKE -> if randomSeed #== tNumber then WINNING else LOSING
    SUCCESS_LOCKED_AMOUNT -> LOSING
    SUCCESS_FINAL -> LOSING
    UNREVEALED_NO_REVEALS -> UNREVEALED_EXPIRED
    UNREVEALED_LOCKED_STAKE_AND_REFUNDS -> if isJust tSecret then EXTRA_REFUNDABLE else UNREVEALED_EXPIRED
    UNREVEALED_LOCKED_REFUNDS -> if isJust tSecret then EXTRA_REFUNDABLE else UNREVEALED_EXPIRED
    UNREVEALED_LOCKED_STAKE -> UNREVEALED_EXPIRED
    UNREVEALED_FINAL -> UNREVEALED_EXPIRED
    _ -> traceError "Ticket should not exist for raffle in this state"
{-# INLINEABLE evalTicketState #-}

checkTicketAction :: RaffleizeAction -> TicketStateId -> Bool
checkTicketAction action currentStateLabel =
  traceIfFalse "Action not permitted in this ticket state" $
    currentStateLabel `pelem` validTicketStatesForRaffleizeAction action
  where
    validTicketStatesForRaffleizeAction :: RaffleizeAction -> [TicketStateId]
    validTicketStatesForRaffleizeAction ra = case ra of
      RaffleOwner roa -> case roa of
        GetCollateralOfExpiredTicket -> [UNREVEALED_EXPIRED]
        _ -> []
      TicketOwner toa -> case toa of
        (RevealTicketSecret _) -> [REVEALABLE]
        CollectStake -> [WINNING]
        RefundTicket -> [FULLY_REFUNDABLE]
        RefundTicketExtra -> [EXTRA_REFUNDABLE]
        RefundCollateralLosing -> [LOSING]
      User _ -> []
      Admin _ -> []
{-# INLINEABLE checkTicketAction #-}

unsafeGetRaffleStateDatumAndValue :: AssetClass -> AddressConstraint -> [PlutusLedgerApi.V3.TxInInfo] -> (PlutusLedgerApi.V3.Value, RaffleStateData)
unsafeGetRaffleStateDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, raffleStateData $ PlutusLedgerApi.V3.unsafeFromBuiltinData b)
{-# INLINEABLE unsafeGetRaffleStateDatumAndValue #-}

unsafeGetTicketStateDatumAndValue :: AssetClass -> AddressConstraint -> [PlutusLedgerApi.V3.TxInInfo] -> (PlutusLedgerApi.V3.Value, TicketStateData)
unsafeGetTicketStateDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, ticketStateData $ PlutusLedgerApi.V3.unsafeFromBuiltinData b)
{-# INLINEABLE unsafeGetTicketStateDatumAndValue #-}

isValidTicketForRaffle :: AssetClass -> TicketStateData -> AssetClass -> Bool
isValidTicketForRaffle ticketUserAC tsd raffleId =
  pand
    [ traceIfFalse "Raffle ids do not match" $
        tRaffle tsd #== raffleId,
      traceIfFalse " Currency symbols do not match" $
        fst (unAssetClass raffleId) #== fst (unAssetClass ticketUserAC) --  -- Must be a ticket of the current raffle.
    ]
{-# INLINEABLE isValidTicketForRaffle #-}

buyTicketToRaffle :: SecretHash -> RaffleStateData -> PlutusLedgerApi.V3.ScriptHash -> (RaffleStateData, TicketStateData)
buyTicketToRaffle sh raffle@RaffleStateData {..} raffleValidator =
  let raffle_data = raffle {rSoldTickets = rSoldTickets #+ 1}
      ticket_data = TicketStateData rSoldTickets sh Nothing rRaffleID raffleValidator
   in (raffle_data, ticket_data)
{-# INLINEABLE buyTicketToRaffle #-}

-- revealTicketToRaffleR :: BuiltinByteString -> TicketStateData -> RaffleStateData -> RaffleStateData
-- revealTicketToRaffleR secret TicketStateData {tSecretHash} raffle@RaffleStateData {rRevealedTickets, rRandomSeed, rSoldTickets} =
--   if blake2b_256 secret #== tSecretHash --  secret matches the secret hash
--     then raffle {rRevealedTickets = rRevealedTickets #+ 1, rRandomSeed = (rRandomSeed #+ bsToInteger' secret) `modInteger` rSoldTickets}
--     else traceError "secret does not match the secret hash"
-- {-# INLINEABLE revealTicketToRaffleR #-}

revealTicketToRaffleT :: BuiltinByteString -> TicketStateData -> TicketStateData
revealTicketToRaffleT secret ticket@TicketStateData {tSecretHash} =
  if blake2b_256 secret #== tSecretHash --  secret matches the secret hash
    then ticket {tSecret = Just secret}
    else traceError "secret does not match the secret hash"
{-# INLINEABLE revealTicketToRaffleT #-}

revealTicketToRaffleRT :: BuiltinByteString -> TicketStateData -> RaffleStateData -> (RaffleStateData, TicketStateData)
revealTicketToRaffleRT secret ticket@TicketStateData {tSecretHash, tRaffle} raffle@RaffleStateData {rRevealedTickets, rRandomSeed, rRaffleID, rSoldTickets} =
  if tRaffle #== rRaffleID && blake2b_256 secret #== tSecretHash -- is correct ticket for the raffle and the secret matches the secret hash
    then
      let updated_ticket = ticket {tSecret = Just secret}
          updated_raffle = raffle {rRevealedTickets = rRevealedTickets #+ 1, rRandomSeed = (rRandomSeed #+ bsToInteger' secret) `modInteger` rSoldTickets}
       in (updated_raffle, updated_ticket)
    else traceError "secret does not match the secret hash"

refundTicketToRaffle :: TicketStateData -> RaffleStateData -> RaffleStateData
refundTicketToRaffle TicketStateData {tRaffle} raffle@RaffleStateData {rRefundedTickets, rRaffleID} =
  if tRaffle #== rRaffleID
    then raffle {rRefundedTickets = rRefundedTickets #+ 1}
    else traceError "ticket raffle id and raffle id does not match"
{-# INLINEABLE refundTicketToRaffle #-}

generateTicketTN :: Integer -> PlutusLedgerApi.V3.TokenName -> PlutusLedgerApi.V3.TokenName
generateTicketTN i (PlutusLedgerApi.V3.TokenName bs) = PlutusLedgerApi.V3.TokenName (takeByteString 28 $ blake2b_256 (bs #<> (serialiseData . PlutusLedgerApi.V3.toBuiltinData) i))
-- TokenName (blake2b_224 (bs #<> (serialiseData . toBuiltinData) i)) --  cheaper than bsToInt
{-# INLINEABLE generateTicketTN #-}

generateTicketAC :: Integer -> AssetClass -> AssetClass
generateTicketAC i (AssetClass (ac, tn)) = AssetClass (ac, generateTicketTN i tn)
{-# INLINEABLE generateTicketAC #-}

deriveUserFromRefTN :: PlutusLedgerApi.V3.TokenName -> PlutusLedgerApi.V3.TokenName
deriveUserFromRefTN (PlutusLedgerApi.V3.TokenName bs) = PlutusLedgerApi.V3.TokenName (userTokenPrefixBS #<> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveUserFromRefTN #-}

deriveRefFromUserTN :: PlutusLedgerApi.V3.TokenName -> PlutusLedgerApi.V3.TokenName
deriveRefFromUserTN (PlutusLedgerApi.V3.TokenName bs) = PlutusLedgerApi.V3.TokenName (refTokenPrefixBS #<> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveRefFromUserTN #-}

deriveUserFromRefAC :: AssetClass -> AssetClass
deriveUserFromRefAC (AssetClass (ac, tn)) = AssetClass (ac, deriveUserFromRefTN tn)
{-# INLINEABLE deriveUserFromRefAC #-}

deriveRefFromUserAC :: AssetClass -> AssetClass
deriveRefFromUserAC (AssetClass (ac, tn)) = AssetClass (ac, deriveRefFromUserTN tn)
{-# INLINEABLE deriveRefFromUserAC #-}

--------------

generateRefAndUserTN :: PlutusLedgerApi.V3.TokenName -> (PlutusLedgerApi.V3.TokenName, PlutusLedgerApi.V3.TokenName)
generateRefAndUserTN (PlutusLedgerApi.V3.TokenName bs) = (PlutusLedgerApi.V3.TokenName (refTokenPrefixBS #<> bs), PlutusLedgerApi.V3.TokenName (userTokenPrefixBS #<> bs))
{-# INLINEABLE generateRefAndUserTN #-}

generateRefAndUserAC :: AssetClass -> (AssetClass, AssetClass)
generateRefAndUserAC (AssetClass (ac, PlutusLedgerApi.V3.TokenName bs)) = (AssetClass (ac, PlutusLedgerApi.V3.TokenName (refTokenPrefixBS #<> bs)), AssetClass (ac, PlutusLedgerApi.V3.TokenName (userTokenPrefixBS #<> bs)))
{-# INLINEABLE generateRefAndUserAC #-}

getNextTicketToMintAssetClasses :: RaffleStateData -> (AssetClass, AssetClass)
getNextTicketToMintAssetClasses RaffleStateData {rRaffleID, rSoldTickets} = generateRefAndUserAC $ generateTicketAC rSoldTickets rRaffleID
{-# INLINEABLE getNextTicketToMintAssetClasses #-}

generateTicketACFromTicket :: TicketStateData -> (AssetClass, AssetClass)
generateTicketACFromTicket TicketStateData {tNumber, tRaffle} = generateRefAndUserAC $ generateTicketAC tNumber tRaffle
{-# INLINEABLE generateTicketACFromTicket #-}

isOneOutputTo :: [PlutusLedgerApi.V3.TxOut] -> PlutusLedgerApi.V3.PubKeyHash -> Bool
isOneOutputTo [out] adminPKH =
  traceIfFalse "The TxOut should be locked to the admin addr" $
    isTxOutWith noConstraint (#== pubKeyHashAddress adminPKH) out
isOneOutputTo _ _ = traceIfFalse "More than one output found" False
{-# INLINEABLE isOneOutputTo #-}

refTokenPrefixBS :: BuiltinByteString
refTokenPrefixBS = integerToBs24 (0x000643b0 :: Integer) -- cheaper
{-# INLINEABLE refTokenPrefixBS #-}

userTokenPrefixBS :: BuiltinByteString
userTokenPrefixBS = integerToBs24 (0x000de140 :: Integer) -- cheaper
{-# INLINEABLE userTokenPrefixBS #-}

-- hasRaffleDatumWithValue :: RaffleDatum -> Value -> Address -> [TxOut] -> Bool
-- hasRaffleDatumWithValue _ _ _ [] = False
-- hasRaffleDatumWithValue rDatum rValue rAddr ((TxOut outAddr outValue (OutputDatum (Datum datum)) Nothing) : outs) =
--   traceIfFalse "raffle state not locked" $
--     pand
--       [ toBuiltinData rDatum #== datum
--       , rValue #== outValue
--       , rAddr #== outAddr
--       ]
--       || hasRaffleDatumWithValue rDatum rValue rAddr outs
-- hasRaffleDatumWithValue _ _ _ _ = False
-- {-# INLINEABLE hasRaffleDatumWithValue #-}

-- hasTicketDatumWithValue :: TicketDatum -> Value -> Address -> [TxOut] -> Bool
-- hasTicketDatumWithValue _ _ _ [] = False
-- hasTicketDatumWithValue ticketDatum tValue tAddr ((TxOut outAddr outValue (OutputDatum (Datum datum)) Nothing) : outs) =
--   traceIfFalse "ticket state not locked" $
--     pand
--       [ toBuiltinData ticketDatum #== datum
--       , tValue #== outValue
--       , tAddr #== outAddr
--       ]
--       || hasTicketDatumWithValue ticketDatum tValue tAddr outs
-- hasTicketDatumWithValue _ _ _ _ = False
-- {-# INLINEABLE hasTicketDatumWithValue #-}

-- paysValueToAddr :: Value -> Address -> [TxOut] -> Bool
-- paysValueToAddr _ _ [] = False
-- paysValueToAddr pValue pAddr ((TxOut outAddr outValue _ _) : outs) =
--   traceIfFalse "value not paid" $
--     pand
--       [ outValue `geq` pValue
--       , pAddr #== outAddr
--       ]
--       || paysValueToAddr pValue pAddr outs
-- {-# INLINEABLE paysValueToAddr #-}

-- Used for Offchain to identify valid utxos before parsing datum
-- This function checks if tokenname has the raffle prefix
hasRefPrefix :: PlutusLedgerApi.V3.TokenName -> Bool
hasRefPrefix (PlutusLedgerApi.V3.TokenName tnbs) = sliceByteString 0 4 tnbs #== refTokenPrefixBS

---------
---------
---------
---------
---------

actionToLabel :: RaffleizeAction -> RaffleizeActionLabel
actionToLabel action = case action of
  User a -> ("User",) $ case a of
    (CreateRaffle _) -> "CreateRaffle"
    (BuyTicket _) -> "BuyTicket"
  RaffleOwner roa -> ("RaffleOwner",) $ case roa of
    (Update _) -> "Update"
    _ -> show roa
  TicketOwner toa -> ("TicketOwner",) $ case toa of
    (RevealTicketSecret _) -> "RevealTicketSecret"
    _ -> show toa
  Admin CloseRaffle -> ("Admin", "CloseRaffle")

validActionLabelsForRaffleState :: RaffleStateId -> [RaffleizeActionLabel]
validActionLabelsForRaffleState = fmap actionToLabel . validActionsForRaffleState
  where
    validActionsForRaffleState :: RaffleStateId -> [RaffleizeAction]
    validActionsForRaffleState r = case r of
      NEW -> [User (BuyTicket mempty), RaffleOwner Cancel, RaffleOwner (Update mempty)]
      EXPIRED_LOCKED_STAKE -> [RaffleOwner RecoverStake]
      EXPIRED_FINAL -> [Admin CloseRaffle]
      COMMITTING -> [User (BuyTicket mempty)]
      UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS -> [RaffleOwner RecoverStake, TicketOwner RefundTicket]
      UNDERFUNDED_LOCKED_REFUNDS -> [TicketOwner RefundTicket]
      UNDERFUNDED_LOCKED_STAKE -> [RaffleOwner RecoverStake]
      UNDERFUNDED_FINAL -> [Admin CloseRaffle]
      REVEALING -> [TicketOwner (RevealTicketSecret mempty)]
      SUCCESS_LOCKED_STAKE_AND_AMOUNT -> [RaffleOwner CollectAmount, TicketOwner CollectStake]
      SUCCESS_LOCKED_AMOUNT -> [RaffleOwner CollectAmount]
      SUCCESS_LOCKED_STAKE -> [TicketOwner CollectStake]
      SUCCESS_FINAL -> [Admin CloseRaffle]
      UNREVEALED_NO_REVEALS -> [RaffleOwner RecoverStakeAndAmount]
      UNREVEALED_LOCKED_STAKE_AND_REFUNDS -> [RaffleOwner RecoverStake, TicketOwner RefundTicketExtra]
      UNREVEALED_LOCKED_REFUNDS -> [TicketOwner RefundTicketExtra]
      UNREVEALED_LOCKED_STAKE -> [RaffleOwner RecoverStake]
      UNREVEALED_FINAL -> [Admin CloseRaffle]

validActionLabelsForTicketState :: TicketStateId -> [RaffleizeActionLabel]
validActionLabelsForTicketState = fmap actionToLabel . validActionsForTicketState
  where
    validActionsForTicketState :: TicketStateId -> [RaffleizeAction]
    validActionsForTicketState r = case r of
      FULLY_REFUNDABLE -> [TicketOwner RefundTicket]
      REVEALABLE -> [TicketOwner (RevealTicketSecret mempty)]
      WINNING -> [TicketOwner CollectStake]
      LOSING -> [TicketOwner RefundCollateralLosing]
      EXTRA_REFUNDABLE -> [TicketOwner RefundTicketExtra]
      UNREVEALED_EXPIRED -> [RaffleOwner GetCollateralOfExpiredTicket]
      _ -> []
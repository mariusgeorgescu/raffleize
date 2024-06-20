module RaffleizeDApp.OnChain.RaffleizeLogic where

import PlutusTx.Builtins (
  blake2b_256,
  divideInteger,
  modInteger,
  serialiseData,
 )

import GHC.Err (error)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (after, before)
import PlutusLedgerApi.V1.Value (AssetClass (..), adaSymbol, adaToken, assetClass, assetClassValueOf, geq, valueOf)
import PlutusLedgerApi.V2 (
  Address,
  Datum (Datum),
  OutputDatum (OutputDatum),
  POSIXTime (POSIXTime),
  POSIXTimeRange,
  PubKeyHash,
  ScriptHash,
  ToData (toBuiltinData),
  TokenName (TokenName),
  TxInInfo,
  TxOut (TxOut),
  UnsafeFromData (unsafeFromBuiltinData),
  Value,
 )
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes (
  RaffleConfig (..),
  RaffleDatum,
  RaffleInfo (..),
  RaffleParam (..),
  RaffleStateData (..),
  RaffleStateId,
  raffleStateData,
 )
import RaffleizeDApp.CustomTypes.TicketTypes (SecretHash, TicketDatum, TicketInfo (TicketInfo), TicketStateData (..), TicketStateId, ticketStateData)
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.OnChain.Utils (AddressConstraint, adaValueFromLovelaces, bsToInteger, getCurrentStateDatumAndValue, integerToBs24, isTxOutWith, noConstraint)
import Prelude hiding (error)

raffleTicketPriceValue :: RaffleStateData -> Value
raffleTicketPriceValue RaffleStateData {rConfig} = adaValueFromLovelaces (rTicketPrice rConfig)
{-# INLINEABLE raffleTicketPriceValue #-}

raffleAccumulatedValue :: RaffleStateData -> Value
raffleAccumulatedValue RaffleStateData {rConfig, rSoldTickets} = adaValueFromLovelaces $ rTicketPrice rConfig #* rSoldTickets
{-# INLINEABLE raffleAccumulatedValue #-}

raffleCollateralValue :: RaffleStateData -> Value
raffleCollateralValue RaffleStateData {rParam} = adaValueFromLovelaces $ rRaffleCollateral rParam
{-# INLINEABLE raffleCollateralValue #-}

raffleTicketCollateralValue :: RaffleStateData -> Value
raffleTicketCollateralValue RaffleStateData {rParam} = adaValueFromLovelaces $ rTicketCollateral rParam
{-# INLINEABLE raffleTicketCollateralValue #-}

checkRaffleParam :: RaffleParam -> Bool
checkRaffleParam RaffleParam {..} =
  pand
    [ rMinRevealingWindow #> 0
    , rMaxNoOfTickets #> 0
    , rTicketCollateral #>= 2_000_000
    , rRaffleCollateral #>= 2_000_000
    ]
{-# INLINEABLE checkRaffleParam #-}

checkRaffle :: RaffleParam -> POSIXTimeRange -> RaffleConfig -> Bool
checkRaffle
  param@RaffleParam {rMinRevealingWindow, rMinTicketPrice}
  timeRange
  RaffleConfig {rRevealDDL, rCommitDDL, rTicketPrice, rMinTickets, rStake} =
    pand
      [ checkRaffleParam param
      , traceIfFalse "invalid commit ddl" $
          rCommitDDL `after` timeRange
      , traceIfFalse "invalid reveal ddl" $
          rRevealDDL #>= rCommitDDL #+ POSIXTime rMinRevealingWindow
      , traceIfFalse "invalid ticket price" $
          rTicketPrice #>= rMinTicketPrice
      , traceIfFalse "invalid min tickets" $
          rMinTickets #> 0
      , traceIfFalse "empty stake" $
          rStake #/= mempty
      , traceIfFalse "stake should not contain ADA" $ -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
      -- to avoid double satisfaction when checking if stake is locked.
          assetClassValueOf rStake (assetClass adaSymbol adaToken) #== 0
      ]
{-# INLINEABLE checkRaffle #-}

showRaffleStateLabel :: RaffleStateId -> String
showRaffleStateLabel r = case r of
  1 -> "NEW"
  10 -> "EXPIRED_LOCKED_STAKE"
  11 -> "EXPIRED_FINAL"
  2 -> "COMMITTING"
  20 -> "UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS"
  21 -> "UNDERFUNDED_LOCKED_REFUNDS"
  22 -> "UNDERFUNDED_LOCKED_STAKE"
  23 -> "UNDERFUNDED_FINAL"
  3 -> "REVEALING"
  40 -> "SUCCESS_LOCKED_STAKE_AND_AMOUNT"
  41 -> "SUCCESS_LOCKED_AMOUNT"
  42 -> "SUCCESS_LOCKED_STAKE"
  43 -> "SUCCESS_FINAL"
  300 -> "UNREVEALED_NO_REVEALS"
  30 -> "UNREVEALED_LOCKED_STAKE_AND_REFUNDS"
  31 -> "UNREVEALED_LOCKED_REFUNDS"
  32 -> "UNREVEALED_LOCKED_STAKE"
  33 -> "UNREVEALED_FINAL"
  _ -> "INVALID STATE"

redeemerToAction :: RaffleizeRedeemer -> RaffleizeAction
redeemerToAction (UserRedeemer action) = User action
redeemerToAction (RaffleOwnerRedeemer action) = RaffleOwner action
redeemerToAction (TicketOwnerRedeemer action _) = TicketOwner action
redeemerToAction (AdminRedeemer action) = Admin action

updateRaffleStateValue :: RaffleizeAction -> RaffleStateData -> Value -> Value
updateRaffleStateValue action rsd@RaffleStateData {rConfig, rSoldTickets, rRevealedTickets} rValue = case action of
  User (BuyTicket _) -> rValue #+ raffleTicketPriceValue rsd
  RaffleOwner RecoverStake -> rValue #- rStake rConfig
  RaffleOwner RecoverStakeAndAmount -> rValue #- rStake rConfig #- raffleAccumulatedValue rsd
  RaffleOwner CollectAmount -> rValue #- raffleAccumulatedValue rsd
  RaffleOwner (Update newconfig) -> (rValue #- rStake rConfig )#+ rStake newconfig
  TicketOwner (RevealTicketSecret _) -> rValue
  TicketOwner CollectStake -> rValue #- rStake rConfig
  TicketOwner RefundTicket ->
    let fullRefundValue = raffleTicketPriceValue rsd #+ raffleTicketCollateralValue rsd
     in rValue #- fullRefundValue
  TicketOwner RefundTicketExtra ->
    let
      extraRefundValue =
        adaValueFromLovelaces (rSoldTickets #* rTicketPrice rConfig `divideInteger` rRevealedTickets)
          #+ raffleTicketCollateralValue rsd
     in
      rValue #- extraRefundValue
  _ -> trace "no raffle state should exist after this action" pmempty
{-# INLINEABLE updateRaffleStateValue #-}

validateRaffleAction :: RaffleizeAction -> RaffleStateId -> Bool
validateRaffleAction action currentStateLabel =
  traceIfFalse "Action not permited in this raffle state" $
    currentStateLabel `pelem` validRaffleStatesForRaffleizeAction action
{-# INLINEABLE validateRaffleAction #-}

validRaffleStatesForRaffleizeAction :: RaffleizeAction -> [RaffleStateId]
validRaffleStatesForRaffleizeAction action = case action of
  User (CreateRaffle _) -> []
  User (BuyTicket _) ->
    [ 1 -- NEW
    , 2 -- COMMIT
    ]
  RaffleOwner roa -> case roa of
    Cancel -> [1] -- NEW -- Current state is valid for cancelling the raffle.
    (Update _) -> [1] -- NEW -- Current state is valid for updating raffle config.
    RecoverStake ->
      [ 10 -- EXPIRED_LOCKED_STAKE
      , 20 -- UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS
      , 22 -- UNDERFUNDED_LOCKED_STAKE
      , 30 -- UNREVEALED_LOCKED_STAKE_AND_REFUNDS
      , 32 -- UNREVEALED_LOCKED_STAKE
      ]
    RecoverStakeAndAmount -> [300] -- UNREVEALED_NO_REVEALS
    CollectAmount ->
      [ 40 -- SUCCESS_LOCKED_STAKE_AND_AMOUNT
      , 41 -- SUCCESS_LOCKED_AMOUNT
      ]
    GetCollateraOfExpiredTicket -> [] -- ticket action, not on raffle state
  TicketOwner toa -> case toa of
    (RevealTicketSecret _) -> [3]
    CollectStake ->
      [ 40 -- SUCCESS_LOCKED_STAKE_AND_AMOUNT
      , 42 -- SUCCESS_LOCKED_STAKE
      ]
    RefundTicket ->
      [ 20 -- UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS
      , 21 -- UNDERFUNDED_LOCKED_REFUNDS.
      ]
    RefundTicketExtra ->
      [ 30 -- UNREVEALED_LOCKED_STAKE_AND_REFUNDS
      , 31 -- UNREVEALED_LOCKED_REFUNDS
      ]
    RefundCollateralLosing -> [] -- ticket action, not on raffle state
  Admin _ ->
    [ 11 -- EXPIRED_FINAL
    , 23 -- UNDERFUNDED_FINAL
    , 33 -- UNREVEALED_FINAL
    , 43 -- SUCCESS_FINAL
    ]
{-# INLINEABLE validRaffleStatesForRaffleizeAction #-}

-- | used in property based testing to check consistency of validActionLabelsForState with  validateRaffleAction
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
validActionLabelsForRaffleState r = case r of
  1 -> [("User", "BuyTicket"), ("RaffleOwner", "Cancel"), ("RaffleOwner", "Update")]
  10 -> [("RaffleOwner", "RecoverStake")]
  11 -> [("Admin", "CloseRaffle")]
  2 -> [("User", "BuyTicket")]
  20 -> [("RaffleOwner", "RecoverStake"), ("TicketOwner", "RefundTicket")]
  21 -> [("TicketOwner", "RefundTicket")]
  22 -> [("RaffleOwner", "RecoverStake")]
  23 -> [("Admin", "CloseRaffle")]
  3 -> [("TicketOwner", "RevealTicketSecret")]
  40 -> [("RaffleOwner", "CollectAmount"), ("TicketOwner", "CollectStake")]
  41 -> [("RaffleOwner", "CollectAmount")]
  42 -> [("TicketOwner", "CollectStake")]
  43 -> [("Admin", "CloseRaffle")]
  300 -> [("RaffleOwner", "RecoverStakeAndAmount")]
  30 -> [("RaffleOwner", "RecoverStake"), ("TicketOwner", "RefundTicketExtra")]
  31 -> [("TicketOwner", "RefundTicketExtra")]
  32 -> [("RaffleOwner", "RecoverStake")]
  33 -> [("Admin", "CloseRaffle")]
  _ -> []

evaluateRaffleState :: (POSIXTimeRange, RaffleStateData, Value) -> RaffleStateId
evaluateRaffleState (time_range, RaffleStateData {rParam, rConfig, rSoldTickets, rRevealedTickets, rRefundedTickets}, svalue) =
  let
    isBeforeCommitDDL = after (rCommitDDL rConfig) time_range
    isBetweenCommitAndRevealDDL = before (rCommitDDL rConfig) time_range && after (rRevealDDL rConfig) time_range
    isStakeLocked = svalue `geq` rStake rConfig
    isCollectedAmmoutLocked = (valueOf svalue adaSymbol adaToken #- rRaffleCollateral rParam) #>= (rTicketPrice rConfig #* rSoldTickets) -- lovelaces wo collateral >  sold ticket * ticket price
    outstandingFullRefunds = rRefundedTickets #< rSoldTickets
    outstandingExtraRefunds = rRefundedTickets #< rRevealedTickets
    anyTicketsSold = rSoldTickets #> 0
    minNoOfTicketsSold = rSoldTickets #< rMinTickets rConfig
    anyTicketsRevealed = rRevealedTickets #> 0
    allTicketsRevealed = rSoldTickets #== rRevealedTickets
   in
    if isBeforeCommitDDL
      then
        if anyTicketsSold
          then 2 -- COMMITTING
          else 1 -- NEW
      else
        if not anyTicketsSold
          then
            if isStakeLocked
              then 10 -- EXPIRED_LOCKED_STAKE
              else 11 -- EXPIRED_FINAL
          else
            if minNoOfTicketsSold
              then -- UNDERFUNDED
              case (isStakeLocked, outstandingFullRefunds) of
                (True, True) -> 20 -- UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS
                (False, True) -> 21 -- UNDERFUNDED_LOCKED_REFUNDS
                (True, False) -> 22 -- UNDERFUNDED_LOCKED_STAKE
                (False, False) -> 23 -- UNDERFUNDED_FINAL
              else
                if allTicketsRevealed
                  then -- SUCCESS
                  case (isStakeLocked, isCollectedAmmoutLocked) of
                    (True, True) -> 40 -- SUCCESS_LOCKED_STAKE_AND_AMOUNT
                    (False, True) -> 41 -- SUCCESS_LOCKED_AMOUNT
                    (True, False) -> 42 -- SUCCESS_LOCKED_STAKE
                    (False, False) -> 43 -- SUCCESS_FINAL
                  else
                    if isBetweenCommitAndRevealDDL
                      then 3 -- REVEALING
                      else case (anyTicketsRevealed, isStakeLocked, outstandingExtraRefunds) of
                        (False, True, False) -> 300 -- UNREVEALED_NO_REVEALS
                        (True, True, True) -> 30 -- UNREVEALED_LOCKED_STAKE_AND_REFUNDS
                        (True, False, True) -> 31 -- UNREVEALED_LOCKED_REFUNDS
                        (True, True, False) -> 32 -- UNREVEALED_LOCKED_STAKE
                        (_, False, False) -> 33 -- UNREVEALED_FINAL
                        (False, _, True) -> traceError "no refunds when 0 tickets are revealed"
{-# INLINEABLE evaluateRaffleState #-}

showTicketStateLabel :: TicketStateId -> String
showTicketStateLabel r = case r of
  90 -> "COMITTED"
  91 -> "FULLY_REFUNDABLE"
  92 -> "REVEALABLE"
  93 -> "REVEALED"
  94 -> "WINNING"
  95 -> "LOSING"
  96 -> "EXTRA_REFUNDABLE"
  97 -> "UNREVEALED_EXPIRED"
  _ -> "INVALID STATE"

evalTicketState :: TicketStateData -> Integer -> RaffleStateId -> TicketStateId
evalTicketState TicketStateData {tNumber, tSecret} randomSeed raffleStateId
  | raffleStateId #== 2 = 90 -- COMITTED
  | raffleStateId #== 10 = 91 -- FULLY_REFUNDABLE
  | raffleStateId #== 20 = 91 -- FULLY_REFUNDABLE
  | raffleStateId #== 21 = 91 -- FULLY_REFUNDABLE
  | raffleStateId #== 3 =
      if isNothing tSecret
        then 92 -- REVEALABLE
        else 93 -- REVEALED
  | raffleStateId #== 40 || raffleStateId #== 42 =
      if randomSeed #== tNumber
        then 94 -- WINNING
        else 95 -- LOSING
  | raffleStateId #== 30 || raffleStateId #== 31 =
      if isJust tSecret
        then 96 -- EXTRA_REFUNDABLE
        else 97 -- UNREVEALED_EXPIRED
        -- \| raffleStateId #== 1 = traceError "Raffle cannot be NEW"
        -- \| raffleStateId #== 11 = traceError "Raffle cannot be EXPIRED_FINAL"
        -- \| raffleStateId #== 22 = traceError "Raffle cannot be UNDERFUNDED_LOCKED_STAKE"
        -- \| raffleStateId #== 23 = traceError "Raffle cannot be UNDERFUNDED_FINAL"
        -- \| raffleStateId #== 32 = traceError "Raffle cannot be UNREVEALED_LOCKED_STAKE"
        -- \| raffleStateId #== 33 = traceError "Raffle cannot be UNREVEALED_LOCKED_FINAL"
        -- \| raffleStateId #== 300 = traceError "Raffle cannot be UNREVEALED_NO_REVEALS"
        -- \| raffleStateId #== 41 = traceError "Raffle cannot be SUCCESS_LOCKED_AMOUNT"
        -- \| raffleStateId #== 43 = traceError "Raffle cannot be SUCCESS_FINAL"
evalTicketState _ _ _ = 9999 -- traceError "invalid state"  - TO DO - SOMETHING ABOUT
{-# INLINEABLE evalTicketState #-}

validActionLabelsForTicketState :: TicketStateId -> [RaffleizeActionLabel]
validActionLabelsForTicketState r = case r of
  91 -> [("TicketOwner", "RefundTicket")]
  92 -> [("TicketOwner", "RevealTicketSecret")]
  94 -> [("TicketOwner", "CollectStake")]
  95 -> [("TicketOwner", "RefundCollateralLosing")]
  96 -> [("TicketOwner", "RefundTicketExtra")]
  97 -> [("RaffleOwner", "GetCollateraOfExpiredTicket")]
  _ -> []

validTicketStatesForRaffleizeAction :: RaffleizeAction -> [TicketStateId]
validTicketStatesForRaffleizeAction ra = case ra of
  RaffleOwner roa -> case roa of
    GetCollateraOfExpiredTicket -> [97] ---- UNREVEALED_EXPIRED   | ticket action only,  not on raffle state
    _ -> []
  TicketOwner toa -> case toa of
    (RevealTicketSecret _) -> [92] -- REVEALABLE
    CollectStake -> [94] -- WINNING
    RefundTicket -> [91] -- FULLY_REFUNDABLE
    RefundTicketExtra -> [96] -- EXTRA_REFUNDABLE
    RefundCollateralLosing -> [95] -- -- LOSING    | ticket action only, not on raffle state
  User _ -> []
  Admin _ -> []

validateTicketAction :: RaffleizeAction -> TicketStateId -> Bool
validateTicketAction action currentStateLabel =
  traceIfFalse "Action not permited in this ticket state" $
    currentStateLabel `pelem` validTicketStatesForRaffleizeAction action
{-# INLINEABLE validateTicketAction #-}

getRaffleStateDatumAndValue :: AssetClass -> AddressConstraint -> [TxInInfo] -> (Value, RaffleStateData)
getRaffleStateDatumAndValue ac addr txins = let (v, b) = getCurrentStateDatumAndValue ac addr txins in (v, raffleStateData $ unsafeFromBuiltinData b)
{-# INLINEABLE getRaffleStateDatumAndValue #-}

getTicketStateDatumAndValue :: AssetClass -> AddressConstraint -> [TxInInfo] -> (Value, TicketStateData)
getTicketStateDatumAndValue ac addr txins = let (v, b) = getCurrentStateDatumAndValue ac addr txins in (v, ticketStateData $ unsafeFromBuiltinData b)
{-# INLINEABLE getTicketStateDatumAndValue #-}

isTicketForRaffle :: AssetClass -> TicketStateData -> RaffleStateData -> Bool
isTicketForRaffle ticketUserAC tsd rsd =
  pand
    [ traceIfFalse "Raffle ids do not match" $
        tRaffle tsd #== rRaffleID rsd
    , traceIfFalse " Currency symbols do not match" $
        fst (unAssetClass (rRaffleID rsd)) #== fst (unAssetClass ticketUserAC) --  -- Must be a ticket of the current raffle.
    ]
{-# INLINEABLE isTicketForRaffle #-}

buyTicketToRaffle :: SecretHash -> RaffleStateData -> ScriptHash -> (RaffleStateData, TicketStateData)
buyTicketToRaffle sh raffle@RaffleStateData {..} raffleValidator =
  let raffle_data = raffle {rSoldTickets = rSoldTickets #+ 1}
      ticket_data = TicketStateData rSoldTickets sh Nothing rRaffleID raffleValidator
   in (raffle_data, ticket_data)
{-# INLINEABLE buyTicketToRaffle #-}

revealTicketToRaffleR :: BuiltinByteString -> TicketStateData -> RaffleStateData -> RaffleStateData
revealTicketToRaffleR secret TicketStateData {tSecretHash} raffle@RaffleStateData {rRevealedTickets, rRandomSeed, rSoldTickets} =
  if blake2b_256 secret #== tSecretHash --  secret matches the secret hash
    then raffle {rRevealedTickets = rRevealedTickets #+ 1, rRandomSeed = (rRandomSeed #+ bsToInteger secret) `modInteger` rSoldTickets}
    else traceError "secret does not match the secret hash"
{-# INLINEABLE revealTicketToRaffleR #-}

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
          updated_raffle = raffle {rRevealedTickets = rRevealedTickets #+ 1, rRandomSeed = (rRandomSeed #+ bsToInteger secret) `modInteger` rSoldTickets}
       in (updated_raffle, updated_ticket)
    else error "secret does not match the secret hash"

refundTicketToRaffle :: TicketStateData -> RaffleStateData -> RaffleStateData
refundTicketToRaffle TicketStateData {tRaffle} raffle@RaffleStateData {rRefundedTickets, rRaffleID} =
  if tRaffle #== rRaffleID
    then raffle {rRefundedTickets = rRefundedTickets #+ 1}
    else traceError "ticket raffle id and raffle id does not match"
{-# INLINEABLE refundTicketToRaffle #-}

generateTicketTN :: Integer -> TokenName -> TokenName
generateTicketTN i (TokenName bs) = TokenName (takeByteString 28 $ blake2b_256 (bs #<> (serialiseData . toBuiltinData) i)) -- TODO integerTOBS
{-# INLINEABLE generateTicketTN #-}

generateTicketAC :: Integer -> AssetClass -> AssetClass
generateTicketAC i (AssetClass (ac, tn)) = AssetClass (ac, generateTicketTN i tn)
{-# INLINEABLE generateTicketAC #-}

deriveUserFromRefTN :: TokenName -> TokenName
deriveUserFromRefTN (TokenName bs) = TokenName (userTokenPrefixBS #<> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveUserFromRefTN #-}

deriveRefFromUserTN :: TokenName -> TokenName
deriveRefFromUserTN (TokenName bs) = TokenName (refTokenPrefixBS #<> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveRefFromUserTN #-}

deriveUserFromRefAC :: AssetClass -> AssetClass
deriveUserFromRefAC (AssetClass (ac, tn)) = AssetClass (ac, deriveUserFromRefTN tn)
{-# INLINEABLE deriveUserFromRefAC #-}

deriveRefFromUserAC :: AssetClass -> AssetClass
deriveRefFromUserAC (AssetClass (ac, tn)) = AssetClass (ac, deriveRefFromUserTN tn)
{-# INLINEABLE deriveRefFromUserAC #-}

--------------

generateRefAndUserTN :: TokenName -> (TokenName, TokenName)
generateRefAndUserTN (TokenName bs) = (TokenName (refTokenPrefixBS #<> bs), TokenName (userTokenPrefixBS #<> bs))
{-# INLINEABLE generateRefAndUserTN #-}

generateRefAndUserAC :: AssetClass -> (AssetClass, AssetClass)
generateRefAndUserAC (AssetClass (ac, TokenName bs)) = (AssetClass (ac, TokenName (refTokenPrefixBS #<> bs)), AssetClass (ac, TokenName (userTokenPrefixBS #<> bs)))
{-# INLINEABLE generateRefAndUserAC #-}

getNextTicketToMintAssetClasses :: RaffleStateData -> (AssetClass, AssetClass)
getNextTicketToMintAssetClasses RaffleStateData {rRaffleID, rSoldTickets} = generateRefAndUserAC $ generateTicketAC rSoldTickets rRaffleID
{-# INLINEABLE getNextTicketToMintAssetClasses #-}

generateTicketACFromTicket :: TicketStateData -> (AssetClass, AssetClass)
generateTicketACFromTicket TicketStateData {tNumber, tRaffle} = generateRefAndUserAC $ generateTicketAC tNumber tRaffle
{-# INLINEABLE generateTicketACFromTicket #-}

isOneOutputTo :: [TxOut] -> PubKeyHash -> Bool
isOneOutputTo [out] adminPKH =
  traceIfFalse "The TxOut should be locked to the addmin addr" $
    isTxOutWith noConstraint (#== pubKeyHashAddress adminPKH) out
isOneOutputTo _ _ = traceIfFalse "More than one ouput found" False
{-# INLINEABLE isOneOutputTo #-}

refTokenPrefixBS :: BuiltinByteString
refTokenPrefixBS = integerToBs24 (0x000643b0 :: Integer)
{-# INLINEABLE refTokenPrefixBS #-}

userTokenPrefixBS :: BuiltinByteString
userTokenPrefixBS = integerToBs24 (0x000de140 :: Integer)
{-# INLINEABLE userTokenPrefixBS #-}

hasRaffleDatumWithValue :: RaffleDatum -> Value -> Address -> [TxOut] -> Bool
hasRaffleDatumWithValue _ _ _ [] = False
hasRaffleDatumWithValue rDatum rValue rAddr ((TxOut outAddr outValue (OutputDatum (Datum datum)) Nothing) : outs) =
  traceIfFalse "raffle state not locked" $
    pand
      [ toBuiltinData rDatum #== datum
      , rValue #== outValue
      , rAddr #== outAddr
      ]
      || hasRaffleDatumWithValue rDatum rValue rAddr outs
hasRaffleDatumWithValue _ _ _ _ = False
{-# INLINEABLE hasRaffleDatumWithValue #-}

hasTicketDatumWithValue :: TicketDatum -> Value -> Address -> [TxOut] -> Bool
hasTicketDatumWithValue _ _ _ [] = False
hasTicketDatumWithValue ticketDatum tValue tAddr ((TxOut outAddr outValue (OutputDatum (Datum datum)) Nothing) : outs) =
  traceIfFalse "ticket state not locked" $
    pand
      [ toBuiltinData ticketDatum #== datum
      , tValue #== outValue
      , tAddr #== outAddr
      ]
      || hasTicketDatumWithValue ticketDatum tValue tAddr outs
hasTicketDatumWithValue _ _ _ _ = False
{-# INLINEABLE hasTicketDatumWithValue #-}

paysValueToAddr :: Value -> Address -> [TxOut] -> Bool
paysValueToAddr _ _ [] = False
paysValueToAddr pValue pAddr ((TxOut outAddr outValue _ _) : outs) =
  traceIfFalse "value not paid" $
    pand
      [ outValue `geq` pValue
      , pAddr #== outAddr
      ]
      || paysValueToAddr pValue pAddr outs
{-# INLINEABLE paysValueToAddr #-}

-- Used for Offchain to identify valid utxos before parsing datum
-- This function checks if tokenname has the raffle prefix
hasRefPrefix :: TokenName -> Bool
hasRefPrefix (TokenName tnbs) = sliceByteString 0 4 tnbs #== refTokenPrefixBS

mkRaffleInfo :: POSIXTimeRange -> (RaffleStateData, Value, String) -> RaffleInfo
mkRaffleInfo tr (rsd, rVal, img) =
  let raffleStateId = evaluateRaffleState (tr, rsd, rVal)
      stateLabel = showRaffleStateLabel raffleStateId
      actions = validActionLabelsForRaffleState raffleStateId
   in RaffleInfo rsd rVal img stateLabel actions

mkTicketInfo :: RaffleStateId -> Integer -> (TicketStateData, Value, String) -> TicketInfo
mkTicketInfo raffleStateId currentRandom (tsd, tVal, tImg) =
  let ticketStateId = evalTicketState tsd currentRandom raffleStateId
      ticketStateLabel = showTicketStateLabel ticketStateId
      actions = validActionLabelsForTicketState ticketStateId
   in TicketInfo tsd tVal tImg ticketStateLabel actions
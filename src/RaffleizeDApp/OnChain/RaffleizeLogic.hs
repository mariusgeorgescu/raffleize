module RaffleizeDApp.OnChain.RaffleizeLogic where

import PlutusTx.Builtins (
  blake2b_256,
  divideInteger,
  modInteger,
 )

import GHC.Err (error)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (after, before)
import PlutusLedgerApi.V1.Value (AssetClass (..), adaSymbol, adaToken, assetClass, assetClassValueOf, geq)
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
  RaffleParam (..),
  RaffleStateData (..),
  RaffleStateLabel,
  raffleStateData,
 )
import RaffleizeDApp.CustomTypes.TicketTypes (SecretHash, TicketDatum, TicketStateData (..), TicketStateLabel, ticketStateData)
import RaffleizeDApp.OnChain.Utils (AddressConstraint, adaValueFromLovelaces, bsToInteger, getCurrentStateDatumAndValue, integerToBs, isTxOutWith, noConstraint)
import Prelude hiding (error)

raffleStakeValue :: RaffleStateData -> Value
raffleStakeValue RaffleStateData {rConfig} = rStake rConfig
{-# INLINEABLE raffleStakeValue #-}

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
          rStake `geq` mempty
      , traceIfFalse "stake should not contain ADA" $ -- to avoid double satisfaction when checking if stake is locked.
          assetClassValueOf rStake (assetClass adaSymbol adaToken) #== 0
      ]
{-# INLINEABLE checkRaffle #-}

showRaffleStateLabel :: RaffleStateLabel -> String
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
  _ -> traceError "invalid"

updateRaffleStateValue :: RaffleizeAction -> RaffleStateData -> Value -> Value
updateRaffleStateValue action rsd@RaffleStateData {rConfig, rSoldTickets, rRevealedTickets} rValue = case action of
  User (BuyTicket _) -> rValue #+ raffleTicketPriceValue rsd
  RaffleOwner RecoverStake -> rValue #- raffleStakeValue rsd
  RaffleOwner RecoverStakeAndAmount -> rValue #- raffleStakeValue rsd #- raffleAccumulatedValue rsd
  RaffleOwner CollectAmount -> rValue #- raffleAccumulatedValue rsd
  RaffleOwner (Update _) -> rValue
  TicketOwner (RevealTicketSecret _) _ -> rValue
  TicketOwner CollectStake _ -> rValue #- raffleStakeValue rsd
  TicketOwner RefundTicket _ ->
    let fullRefundValue = raffleTicketPriceValue rsd #+ raffleTicketCollateralValue rsd
     in rValue #- fullRefundValue
  TicketOwner RefundTicketExtra _ ->
    let
      extraRefundValue =
        adaValueFromLovelaces (rSoldTickets #* rTicketPrice rConfig `divideInteger` rRevealedTickets)
          #+ raffleTicketCollateralValue rsd
     in
      rValue #- extraRefundValue
  _ -> trace "no raffle state should exist after this action" pmempty
{-# INLINEABLE updateRaffleStateValue #-}

validateRaffleAction :: RaffleizeAction -> Integer -> Bool
validateRaffleAction action currentStateLabel =
  let invalidActionError = "invalid action for validator"
   in traceIfFalse "Action not permited in this raffle state" $
        currentStateLabel `pelem` case action of
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
            GetCollateraOfExpiredTicket -> traceError invalidActionError
          TicketOwner toa _ -> case toa of
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
            RefundCollateralLosing -> traceError invalidActionError
          Admin _ ->
            [ 11 -- EXPIRED_FINAL
            , 23 -- UNDERFUNDED_FINAL
            , 33 -- UNREVEALED_FINAL
            , 43 -- SUCCESS_FINAL
            ]
{-# INLINEABLE validateRaffleAction #-}

evaluateRaffleState :: (POSIXTimeRange, RaffleStateData, Value) -> RaffleStateLabel
evaluateRaffleState (time_range, rsd@RaffleStateData {rConfig, rSoldTickets, rRevealedTickets, rRefundedTickets}, svalue) =
  let isBeforeCommitDDL = after (rCommitDDL rConfig) time_range
      isBetweenCommitAndRevealDDL = before (rCommitDDL rConfig) time_range && after (rRevealDDL rConfig) time_range
      isStakeLocked = svalue `geq` rStake rConfig 
      isCollectedAmmoutLocked = svalue `geq` raffleAccumulatedValue rsd
      outstandingFullRefunds = rRefundedTickets #< rSoldTickets
      outstandingExtraRefunds = rRefundedTickets #< rRevealedTickets
      anyTicketsSold = rSoldTickets #> 0
      minNoOfTicketsSold = rSoldTickets #< rMinTickets rConfig
      anyTicketsRevealed = rRevealedTickets #> 0
      allTicketsRevealed = rSoldTickets #== rRevealedTickets
   in if isBeforeCommitDDL
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

evalTicketState :: TicketStateData -> RaffleStateLabel -> TicketStateLabel
evalTicketState tsd rs
  | rs #== 3 -- REVEALING
    =
      5 -- ==> REVEALABLE
  | rs
      `pelem` [ 20 -- UNDERFUNDED_LOCKED_STAKE_AND_REFUNDS
              , 21 -- UNDERFUNDED_LOCKED_REFUNDS
              , 40 -- SUCCESS_LOCKED_STAKE_AND_AMOUNT
              , 42 -- SUCCESS_LOCKED_STAKE
              ] =
      6 --  ==> BURNABLE_BY_TICKET_OWNER
  | rs
      `pelem` [ 30 -- UNREVEALED_LOCKED_STAKE_AND_REFUNDS
              , 31 -- UNREVEALED_LOCKED_REFUNDS
              ] =
      if isJust (tSecret tsd) --- revealed ticket
        then 6 -- ==> BURNABLE_BY_TICKET_OWNER
        else 7 -- ==> BURNABLE_BY_RAFFLE_OWNER
  | otherwise = 0 -- LOCKED ==> UNSPENDABLE
{-# INLINEABLE evalTicketState #-}

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
    then raffle {rRefundedTickets = rRefundedTickets #- 1}
    else traceError "ticket raffle id and raffle id does not match"
{-# INLINEABLE refundTicketToRaffle #-}

generateTicketTN :: Integer -> TokenName -> TokenName
generateTicketTN i (TokenName bs) = TokenName (takeByteString 28 $ blake2b_256 (bs #<> integerToBs i))
{-# INLINEABLE generateTicketTN #-}

generateTicketAC :: Integer -> AssetClass -> AssetClass
generateTicketAC i (AssetClass (ac, tn)) = AssetClass (ac, generateTicketTN i tn)
{-# INLINEABLE generateTicketAC #-}

deriveUserFromRefTN :: TokenName -> TokenName
deriveUserFromRefTN (TokenName bs) = TokenName (userTokenPrefixBS #<> sliceByteString 4 (lengthOfByteString bs) bs)
{-# INLINEABLE deriveUserFromRefTN #-}

deriveUserFromRefAC :: AssetClass -> AssetClass
deriveUserFromRefAC (AssetClass (ac, tn)) = AssetClass (ac, deriveUserFromRefTN tn)
{-# INLINEABLE deriveUserFromRefAC #-}

--------------

generateRefAndUserTN :: TokenName -> (TokenName, TokenName)
generateRefAndUserTN (TokenName bs) = (TokenName (refTokenPrefxBS #<> bs), TokenName (userTokenPrefixBS #<> bs))
{-# INLINEABLE generateRefAndUserTN #-}

generateRefAndUserAC :: AssetClass -> (AssetClass, AssetClass)
generateRefAndUserAC (AssetClass (ac, TokenName bs)) = (AssetClass (ac, TokenName (refTokenPrefxBS #<> bs)), AssetClass (ac, TokenName (userTokenPrefixBS #<> bs)))
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

refTokenPrefxBS :: BuiltinByteString
refTokenPrefxBS = integerToBs (0x000643b0 :: Integer)
{-# INLINEABLE refTokenPrefxBS #-}

userTokenPrefixBS :: BuiltinByteString
userTokenPrefixBS = integerToBs (0x000de140 :: Integer)
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

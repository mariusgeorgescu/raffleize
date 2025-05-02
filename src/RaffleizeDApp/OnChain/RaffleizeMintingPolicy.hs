-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module RaffleizeDApp.OnChain.RaffleizeMintingPolicy where

import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue)
import PlutusLedgerApi.V3
  ( FromData (fromBuiltinData),
    Redeemer (Redeemer),
    TxOutRef,
    Value (getValue),
  )
import PlutusLedgerApi.V3.Contexts
  ( ScriptInfo (MintingScript),
  )
import PlutusTx
  ( CompiledCode,
    compile,
    liftCode,
    unsafeApplyCode,
    unstableMakeIsData,
  )
import PlutusTx.AssocMap qualified as M
import RaffleizeDApp.CustomTypes.ActionTypes (RaffleizeAction (..), UserAction (BuyTicket))
import RaffleizeDApp.CustomTypes.RaffleTypes
  ( RaffleConfig (RaffleConfig, rStake),
    RaffleParam (RaffleParam, rRaffleCollateral, rRaffleValidatorHash, rTicketValidatorHash),
    RaffleStateData,
    RaffleStateId (COMMITTING, NEW),
    mkNewRaffle,
    mkRaffleDatum,
  )
import RaffleizeDApp.CustomTypes.TicketTypes (SecretHash, TicketStateData, mkTicketDatum)
import RaffleizeDApp.OnChain.RaffleizeLogic
  ( buyTicketToRaffle,
    checkRaffleConfig,
    evaluateRaffleState,
    generateRefAndUserAC,
    getNextTicketToMintAssetClasses,
    ticketCollateralValue,
    tokenNameFromTxOutRef,
    unsafeGetRaffleStateDatumAndValue,
    updateRaffleStateValue,
  )
import RaffleizeDApp.OnChain.Utils
  ( AScriptContext (AScriptContext),
    ATxInfo (..),
    adaValueFromLovelaces,
    hasTxInWithRef,
    hasTxOutWithInlineDatumAnd,
    mkUntypedLambda,
  )

-- | Custom redeemer type to indicate minting mode.
data RaffleizeMintingReedemer
  = MintRaffle RaffleConfig TxOutRef
  | MintTicket AssetClass SecretHash
  | Burn
  deriving (Generic)

unstableMakeIsData ''RaffleizeMintingReedemer ---  must be changed with stable version

raffleizePolicyLambda :: RaffleParam -> AScriptContext -> Bool --- TODO : Updatable params-> Move raffle params to a reference input with inline datum
raffleizePolicyLambda params@RaffleParam {rRaffleValidatorHash, rTicketValidatorHash, rRaffleCollateral} (AScriptContext ATxInfo {..} (Redeemer bredeemer) (MintingScript cs)) =
  let raffleValidatorAddress = scriptHashAddress rRaffleValidatorHash
   in case fromBuiltinData bredeemer of
        Nothing -> traceError "invalid redeemer"
        Just raffleizeMintingRedeemer ->
          case raffleizeMintingRedeemer of
            (MintRaffle config@RaffleConfig {rStake} seedTxOutRef) ->
              let (!raffleRefAC, !raffleUserAC) = generateRefAndUserAC $ AssetClass (cs, tokenNameFromTxOutRef seedTxOutRef)
                  !raffleRefNFT = assetClassValue raffleRefAC 1
                  !raffleUserNFT = assetClassValue raffleUserAC 1
               in pand
                    [ traceIfFalse "Raffle configuration must be valid" $
                        checkRaffleConfig params txInfoValidRange config,
                      traceIfFalse "Tx must spend seed utxo used to generate RaffleID" $
                        hasTxInWithRef seedTxOutRef txInfoInputs,
                      traceIfFalse "Tx must mint JUST raffle's ref and user NFTs" $
                        txInfoMint #== (raffleRefNFT #+ raffleUserNFT),
                      traceIfFalse "Tx must lock new raffle state at RaffleValidator address (with correct value and datum)" $
                        hasTxOutWithInlineDatumAnd
                          (mkRaffleDatum $ mkNewRaffle raffleRefAC params config)
                          (#== (raffleRefNFT #+ rStake #+ adaValueFromLovelaces rRaffleCollateral))
                          (#== raffleValidatorAddress)
                          txInfoOutputs
                    ]
            (MintTicket raffleID secretHash) ->
              let (!currentValue, !rsd) = unsafeGetRaffleStateDatumAndValue raffleID (#== raffleValidatorAddress) txInfoInputs -- Fails if raffle does not exists
                  !raffleValidatorAddr = scriptHashAddress rRaffleValidatorHash
                  !ticketValidatorAddr = scriptHashAddress rTicketValidatorHash
                  (!new_rsd, !new_tsd) = buyTicketToRaffle secretHash rsd rRaffleValidatorHash
                  (!ticketRefAC, !ticketUserAC) = getNextTicketToMintAssetClasses rsd -- Generate tickets  based on no. of tickets sold.
                  !ticketRefNFT = assetClassValue ticketRefAC 1
                  !ticketUserNFT = assetClassValue ticketUserAC 1
                  !updatedRaffleStateValue = updateRaffleStateValue (User (BuyTicket secretHash)) rsd currentValue
                  locksRaffleStateWithUpdatedDatumAndValue (newStateData :: RaffleStateData) =
                    hasTxOutWithInlineDatumAnd (mkRaffleDatum newStateData) (#== updatedRaffleStateValue) (#== raffleValidatorAddr) txInfoOutputs -- using (==) to avoid utxo value ddos
                  locksNewTicketState (newStateData :: TicketStateData) newValue =
                    hasTxOutWithInlineDatumAnd (mkTicketDatum newStateData) newValue (#== ticketValidatorAddr) txInfoOutputs
               in pand
                    [ traceIfFalse "Not SHA256 hash" $ lengthOfByteString secretHash #== 64,
                      traceIfFalse "Tx must spend a raffle  with valid state for minting tickets" $
                        evaluateRaffleState (txInfoValidRange, rsd, currentValue) `pelem` [NEW, COMMITTING],
                      traceIfFalse "Tx must update the raffle state" $
                        locksRaffleStateWithUpdatedDatumAndValue new_rsd, -- Must lock raffle state at Raffle Validator address (with updated value and datum).
                      traceIfFalse "Tx must create ticket th new ticket's state" $
                        locksNewTicketState
                          new_tsd
                          (#== (ticketRefNFT #+ ticketCollateralValue rsd)), -- Must lock ticket state at Ticket Validator address (with collateral value and valid datum).
                      traceIfFalse "Tx must mint JUST ticket's ref and user NFTs" $
                        txInfoMint #== (ticketRefNFT #+ ticketUserNFT)
                    ]
            Burn ->
              let mTokens = M.toList <$> M.lookup cs (getValue txInfoMint) -- All negative
               in case mTokens of
                    Nothing -> traceError "Impossible: current currency symbol not in txInfoMint"
                    Just tokens ->
                      let justBurning = pand $ ((#< 0) . snd) #<$> tokens
                       in traceIfFalse
                            "Tx must JUST burn tokens of the current Currency Symbol"
                            justBurning
raffleizePolicyLambda _ _ = traceError "Must be used only for minting purpose!"
{-# INLINEABLE raffleizePolicyLambda #-}

untypedRaffleizePolicyLambda :: RaffleParam -> BuiltinData -> BuiltinUnit
untypedRaffleizePolicyLambda p = mkUntypedLambda $ raffleizePolicyLambda p
{-# INLINEABLE untypedRaffleizePolicyLambda #-}

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileRaffleizeMP :: RaffleParam -> CompiledCode (BuiltinData -> BuiltinUnit)
compileRaffleizeMP p = $$(compile [||untypedRaffleizePolicyLambda||]) `unsafeApplyCode` liftCode plcVersion110 p

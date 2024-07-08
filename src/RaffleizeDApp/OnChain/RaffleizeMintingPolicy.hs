{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module RaffleizeDApp.OnChain.RaffleizeMintingPolicy where

import PlutusCore.Builtin.Debug (plcVersion100)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue)
import PlutusLedgerApi.V2 (
  ScriptPurpose (Minting),
  TxOutRef,
 )
import PlutusTx (
  CompiledCode,
  compile,
  liftCode,
  unsafeApplyCode,
  unstableMakeIsData,
 )
import RaffleizeDApp.CustomTypes.RaffleTypes (
  RaffleConfig (RaffleConfig, rStake),
  RaffleParam (RaffleParam, rRaffleCollateral, rRaffleValidatorHash),
  mkNewRaffle,
  mkRaffleDatum,
 )
import RaffleizeDApp.CustomTypes.Types (
  AScriptContext (AScriptContext),
  ATxInfo (..),
 )
import RaffleizeDApp.OnChain.RaffleizeLogic (
  checkRaffle,
  evaluateRaffleState,
  generateRefAndUserAC,
  getRaffleStateDatumAndValue,
 )
import RaffleizeDApp.OnChain.Utils (
  adaValueFromLovelaces,
  hasTxInWithRef,
  hasTxOutWithInlineDatumAnd,
  isBurningNFT,
  isMintingNFT,
  mkUntypedMintingPolicyCustom,
  tokenNameFromTxOutRef,
 )

-- | Custom redeemer type to indicate minting mode.
data RaffleizeMintingReedemer
  = MintRaffle RaffleConfig TxOutRef
  | MintTicket AssetClass
  | Burn AssetClass
  deriving (Generic)

unstableMakeIsData ''RaffleizeMintingReedemer ---  must be changed with stable version

raffleizePolicyLambda :: RaffleParam -> RaffleizeMintingReedemer -> AScriptContext -> Bool --- TODO : Updatable params-> Move raffle params to a reference input with inline datum
raffleizePolicyLambda param@RaffleParam {rRaffleValidatorHash, rRaffleCollateral} redeemer (AScriptContext ATxInfo {..} (Minting cs)) =
  let raffleValidatorAddress = scriptHashAddress rRaffleValidatorHash
   in case redeemer of
        MintRaffle config@RaffleConfig {rStake} seedTxOutRef ->
          let (!raffleRefAC, !raffleUserAC) = generateRefAndUserAC $ AssetClass (cs, tokenNameFromTxOutRef seedTxOutRef)
              raffleRefTokenValue = assetClassValue raffleRefAC 1
              raffleUserTokenValue = assetClassValue raffleUserAC 1
              !newRaffleDatum = mkRaffleDatum $ mkNewRaffle raffleRefAC param config
           in pand
                [ traceIfFalse "Must be valid configuration" $
                    checkRaffle param txInfoValidRange config
                , traceIfFalse "Must spend seed utxo" $
                    hasTxInWithRef seedTxOutRef txInfoInputs
                , traceIfFalse "Must mint the ref NFT" $
                    isMintingNFT raffleRefAC txInfoMint
                , traceIfFalse "Must mint the user NFT" $
                    isMintingNFT raffleUserAC txInfoMint
                , traceIfFalse "Must lock new raffle state at Raffle Validator (with correct value and datum)" $
                    hasTxOutWithInlineDatumAnd newRaffleDatum (#== (raffleRefTokenValue #+ rStake #+ adaValueFromLovelaces rRaffleCollateral)) (#== raffleValidatorAddress) txInfoOutputs
                ]
        MintTicket raffleID ->
          let raffleRefToken = assetClassValue raffleID 1
              (currentValue, rsd) = getRaffleStateDatumAndValue raffleID (#== raffleValidatorAddress) txInfoInputs -- TO DOOOO de updatat tipul datum
           in evaluateRaffleState (txInfoValidRange, rsd, currentValue) `pelem` [1, 2]
        Burn ac -> isBurningNFT ac txInfoMint
raffleizePolicyLambda _ _ _ = traceError "invalid purpose"
{-# INLINEABLE raffleizePolicyLambda #-}

untypedRaffleizePolicyLambda :: RaffleParam -> BuiltinData -> BuiltinData -> ()
untypedRaffleizePolicyLambda p = mkUntypedMintingPolicyCustom $ raffleizePolicyLambda p
{-# INLINEABLE untypedRaffleizePolicyLambda #-}

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileRaffleizeMP :: RaffleParam -> CompiledCode (BuiltinData -> BuiltinData -> ())
compileRaffleizeMP p = $$(compile [||untypedRaffleizePolicyLambda||]) `unsafeApplyCode` liftCode plcVersion100 p

-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module RaffleizeDApp.OnChain.NFT where

import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1 (AssetClass (AssetClass), flattenValue, scriptHashAddress)
import PlutusLedgerApi.V1.Value (assetClassValue, geq)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.AssocMap
import PlutusTx.Blueprint
import RaffleizeDApp.Constants (metadataVersion)
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.OnChain.Utils

-- | Define a custom type to parameterize the minting policy:
data PolicyParam = PolicyParam
  { getUtxo :: TxOutRef,
    getTokenName :: TokenName
  }

-- | Generate `Lift` instance for the custom parameter type with Template Haskell.
--   Allows argument value to be pre-compiled to UPLC, so the compiled parameterized script can be applied to it.
makeLift ''PolicyParam

-- | Custom redeemer type to indicate minting mode.
data TokenData
  = TokenData
  { tdName :: BuiltinByteString,
    tdDesc :: BuiltinByteString,
    tdImageURI :: BuiltinByteString,
    tdAuthor :: BuiltinByteString
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

makeIsDataSchemaIndexed ''TokenData [('TokenData, 0)]

data NFTAction = MintingNFT TokenData | BurningNFT
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | For custom types with multiple constructors, `makeIsDataIndexed` must be used to generate ToData/FromData instances.
--   Unlike `unstableMakeIsData`, this generates `BuiltinData` values with constructors indexed in a stable order.
makeIsDataIndexed ''NFTAction [('MintingNFT, 0), ('BurningNFT, 1)]

----------------
-- NFTs METADATA
----------------

-- | The datum datatype which should be locked with the batch ref NFT.
-- | This datatype is following the CIP-68 Datum Metadata Standard.
data CIP68Datum = CIP68Datum
  { metadata :: Metadata, --- ^  Map k v (where k and v arr  UTF-8 encoded @BuiltinByteString@s)
    version :: Integer, --- ^ version of CIP-68 Datum Metadata Standard.
    extra :: ()
  }
  ---- ^ Plutus data

  deriving (Generic, Show)

makeIsDataSchemaIndexed ''CIP68Datum [('CIP68Datum, 0)]

mkCIP68Datum :: TokenData -> CIP68Datum
mkCIP68Datum TokenData {..} =
  CIP68Datum
    { metadata =
        safeFromList
          [ (encodeUtf8 "description", tdDesc),
            (encodeUtf8 "image", tdImageURI),
            (encodeUtf8 "name", tdName),
            (encodeUtf8 "author", tdAuthor)
          ],
      version = metadataVersion,
      extra = ()
    }

-- 2. Define Lambda

-- | Helper function to check if the correct quantity of the given asset is minted in the transaction.
checkMintAmount :: CurrencySymbol -> TokenName -> NFTAction -> Value -> Bool
checkMintAmount cs tn mode minted =
  let q = case mode of
        MintingNFT _ -> 1
        BurningNFT -> -1
   in pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q' #== q) $ flattenValue minted
{-# INLINEABLE checkMintAmount #-}

-- | Typed parameterized minting policy lambda.
nftLambda :: PolicyParam -> AScriptContext -> Bool
nftLambda (PolicyParam oref tn) (AScriptContext ATxInfo {..} (Redeemer bredeemer) (MintingScript cs@(CurrencySymbol bshash))) =
  case fromBuiltinData bredeemer of
    Nothing -> traceError "invalid redeemer"
    Just mode ->
      let (!refAC, !userAC) = generateRefAndUserAC $ AssetClass (cs, tn)
          !refNFT = assetClassValue refAC 1
          !userNFT = assetClassValue userAC 1
       in -- check a list of conditions that must be met to mint with the policy
          pand $
            traceIfFalse
              "Tx must mint JUST raffle's ref and user NFTs"
              (txInfoMint #== (refNFT #+ userNFT))
              : case mode of
                -- if minting, the UTxO parameterizing the policy must be consumed as an input
                MintingNFT token_data ->
                  [ traceIfFalse "UTxO not consumed" $ hasTxInWithRef oref txInfoInputs,
                    hasTxOutWithInlineDatumAnd
                      (mkCIP68Datum token_data) -- has corect inline datum
                      (`geq` refNFT) -- has ref token
                      (#== scriptHashAddress (ScriptHash bshash)) -- is locked to validator
                      txInfoOutputs
                  ]
                BurningNFT -> [] -- if burning, no additional conditions are required: construct a singleton list with empty tail
nftLambda _ _ = False -- TODO The spending part ..with user
{-# INLINEABLE nftLambda #-}

-- | Lose types
nftUntyped :: PolicyParam -> BuiltinData -> BuiltinUnit
nftUntyped policyParm =
  mkUntypedLambda (nftLambda policyParm)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
nftCompile :: PolicyParam -> CompiledCode (BuiltinData -> BuiltinUnit)
nftCompile mp =
  $$(compile [||nftUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 mp
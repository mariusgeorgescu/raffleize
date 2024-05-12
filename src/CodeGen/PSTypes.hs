module CodeGen.PSTypes where

import Control.Lens
import Control.Monad.Reader.Class (MonadReader)
import Language.PureScript.Bridge.Builder
import Language.PureScript.Bridge.TypeInfo
import Prelude

-- psInt
-- Data.DateTime.Instant

-- | Use type definition in Raffleize.Types
psClientType :: MonadReader BridgeData m => m PSType
psClientType = do
  inType <- view haskType
  params <- psTypeParameters
  return
    TypeInfo
      { _typePackage = ""
      , _typeModule = "Raffleize.Types"
      , _typeName = inType ^. typeName
      , _typeParameters = params
      }

-- | Use type definition in Csl
psCslValue :: TypeInfo 'PureScript
psCslValue =
  TypeInfo
    { _typePackage = "cardano-serialization-lib"
    , _typeModule = "Csl"
    , _typeName = "ValueJson"
    , _typeParameters = []
    }

-- | Use type definition in Csl
psCslAssetClass :: TypeInfo 'PureScript
psCslAssetClass =
  TypeInfo
    { _typePackage = "cardano-serialization-lib"
    , _typeModule = "Csl"
    , _typeName = "AssetClass"
    , _typeParameters = []
    }

module CodeGen.PSTypes where

import Control.Lens
import Control.Monad.Reader.Class (MonadReader)
import Language.PureScript.Bridge.Builder
import Language.PureScript.Bridge.TypeInfo
import Prelude


-- -- | ....
-- psInstant :: TypeInfo 'PureScript
-- psInstant = TypeInfo {
--     _typePackage = ""
--   , _typeModule = "Data.DateTime.Instant"
--   , _typeName = "Instant"
--   , _typeParameters = []
--   }


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

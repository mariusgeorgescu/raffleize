module PSTypes where

import Control.Lens (view, (^.))
import Control.Monad.Reader.Class (MonadReader)
import Language.PureScript.Bridge.Builder (
  BridgeData,
  psTypeParameters,
 )
import Language.PureScript.Bridge.TypeInfo (
  HasHaskType (haskType),
  PSType,
  TypeInfo (
    TypeInfo,
    _typeModule,
    _typeName,
    _typePackage,
    _typeParameters
  ),
  typeName,
 )
import Prelude

------------------------------------------------------------------------------------------------

-- *  Using type definitions from the purescript project

------------------------------------------------------------------------------------------------

-- | Use type definition in Raffleize.Types
psClientType :: (MonadReader BridgeData m) => m PSType
psClientType = do
  inType <- view haskType
  params <- psTypeParameters
  return
    TypeInfo
      { _typePackage = ""
      , _typeModule = "Raffleize.Value"
      , _typeName = inType ^. typeName
      , _typeParameters = params
      }

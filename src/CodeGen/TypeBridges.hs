module CodeGen.TypeBridges where

import CodeGen.PSTypes (psClientType)
import Language.PureScript.Bridge (
  BridgePart,
  defaultBridge,
  typeName,
  (^==),
 )
import Language.PureScript.Bridge.PSTypes (
  psInt,
  psNumber,
  psString,
 )

------------------------------------------------------------------------------------------------

-- *  Haskell to Purescript Type Bridges

------------------------------------------------------------------------------------------------

-- | Mapping haskell 'POSIXTime' to purescript 'Number'
posixTimeBridge :: BridgePart
posixTimeBridge = do
  typeName ^== "POSIXTime"
  return psNumber

-- | Mapping haskell 'Integer' to purescript 'Int'
plutusIntegerBridge :: BridgePart
plutusIntegerBridge = do
  typeName ^== "Integer"
  return psInt

-- | Mapping haskell 'Value' to purescript type defined in the purescript project
plutusValueBridge :: BridgePart
plutusValueBridge = do
  typeName ^== "Value"
  psClientType

-- | Mapping haskell 'AssetClass' to purescript type defined in the purescript project
plutusAssetClassBridge :: BridgePart
plutusAssetClassBridge = do
  typeName ^== "AssetClass"
  psClientType

-- | Mapping haskell 'BuiltinByteString' to purescript 'String'
plutusBuiltinByteStringBridge :: BridgePart
plutusBuiltinByteStringBridge = do
  typeName ^== "BuiltinByteString"
  return psString

-- | Mapping haskell 'ScriptHash' to purescript 'String'
plutusScriptHashBridge :: BridgePart
plutusScriptHashBridge = do
  typeName ^== "ScriptHash"
  return psString

-- | Mapping haskell 'GYAddress' to purescript 'String'
plutusGYAddressBridge :: BridgePart
plutusGYAddressBridge = do
  typeName ^== "GYAddress"
  return psString

-- | Mapping haskell 'GYTxOutRefCbor' to purescript 'String'
plutusGYTxOutRefCborBridge :: BridgePart
plutusGYTxOutRefCborBridge = do
  typeName ^== "GYTxOutRefCbor"
  return psString

-- | Raffleize Purescript
raffleizeBridge :: BridgePart
raffleizeBridge =
  defaultBridge
    <|> posixTimeBridge
    <|> plutusIntegerBridge
    <|> plutusValueBridge
    <|> plutusBuiltinByteStringBridge
    <|> plutusScriptHashBridge
    <|> plutusAssetClassBridge
    <|> plutusGYAddressBridge
    <|> plutusGYTxOutRefCborBridge
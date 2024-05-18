module CodeGen.TypeBridges where

import CodeGen.PSTypes (psClientType)
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

posixTimeBridge :: BridgePart
posixTimeBridge = do
  typeName ^== "POSIXTime"
  return psNumber 

plutusIntegerBridge :: BridgePart
plutusIntegerBridge = do
  typeName ^== "Integer"
  return psInt

plutusValueBridge :: BridgePart
plutusValueBridge = do
  typeName ^== "Value"
  psClientType

plutusBuiltinByteStringBridge :: BridgePart
plutusBuiltinByteStringBridge = do
  typeName ^== "BuiltinByteString"
  return psString

plutusScriptHashBridge :: BridgePart
plutusScriptHashBridge = do
  typeName ^== "ScriptHash"
  return psString

plutusAssetClassBridge :: BridgePart
plutusAssetClassBridge = do
  typeName ^== "AssetClass"
  psClientType

plutusGYAddressBridge :: BridgePart
plutusGYAddressBridge = do
  typeName ^== "GYAddress"
  return psString

plutusGYTxOutRefCborBridge :: BridgePart
plutusGYTxOutRefCborBridge = do
  typeName ^== "GYTxOutRefCbor"
  return psString

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
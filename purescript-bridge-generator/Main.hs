{-# LANGUAGE DataKinds #-}

module Main where

import CodeGen.TypeBridges (raffleizeBridge)
import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge (
  Language (Haskell),
  SumType,
  buildBridge,
  equal,
  mkSumType,
  writePSTypes,
 )
import RaffleizeDApp.Constants (purescriptProjectSrcPath)
import RaffleizeDApp.CustomTypes.ActionTypes (
  AdminAction,
  RaffleOwnerAction,
  RaffleizeAction,
  TicketOwnerAction,
  UserAction,
 )
import RaffleizeDApp.CustomTypes.RaffleTypes (
  RaffleConfig,
  RaffleInfo,
  RaffleParam,
  RaffleStateData,
 )
import RaffleizeDApp.CustomTypes.TicketTypes (TicketStateData)
import RaffleizeDApp.CustomTypes.TransferTypes (
  RaffleizeInteraction,
  UserAddresses,
 )

myTypes :: [SumType 'Haskell]
myTypes =
  [ let p = (Proxy :: Proxy RaffleConfig) in equal p (mkSumType p)
  , mkSumType (Proxy :: Proxy RaffleParam)
  , mkSumType (Proxy :: Proxy RaffleStateData)
  , mkSumType (Proxy :: Proxy TicketStateData)
  , mkSumType (Proxy :: Proxy UserAction)
  , mkSumType (Proxy :: Proxy TicketOwnerAction)
  , mkSumType (Proxy :: Proxy RaffleOwnerAction)
  , mkSumType (Proxy :: Proxy AdminAction)
  , mkSumType (Proxy :: Proxy RaffleizeAction)
  , mkSumType (Proxy :: Proxy UserAddresses)
  , mkSumType (Proxy :: Proxy RaffleizeInteraction)
  , mkSumType (Proxy :: Proxy RaffleInfo)
  ]

main :: IO ()
main = do
  writePSTypes purescriptProjectSrcPath (buildBridge raffleizeBridge) myTypes
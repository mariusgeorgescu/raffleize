{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import CodeGen.TypeBridges
import Data.Proxy
import Language.PureScript.Bridge
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.TxBuilding.Context  

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy RaffleConfig)
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
  ]

main :: IO ()
main = do
  writePSTypes "../raffleize-frontend-purescript/src/" (buildBridge raffleizeBridge) myTypes
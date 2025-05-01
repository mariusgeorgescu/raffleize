module Main where

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge
  ( Language (Haskell),
    SumType,
    buildBridge,
    equal,
    mkSumType,
    writePSTypes,
  )
import RaffleizeDApp.Constants (purescriptProjectSrcPath)
import RaffleizeDApp.CustomTypes.ActionTypes
  ( AdminAction,
    RaffleOwnerAction,
    RaffleizeAction,
    TicketOwnerAction,
    UserAction,
  )
import RaffleizeDApp.CustomTypes.RaffleTypes
  ( RaffleConfig,
    RaffleParam,
    RaffleStateData,
  )
import RaffleizeDApp.CustomTypes.TicketTypes (TicketStateData)
import RaffleizeDApp.CustomTypes.TransferTypes
  ( AddWitAndSubmitParams,
    RaffleInfo,
    RaffleizeInteraction,
    TicketInfo,
    UserAddresses,
  )
import TypeBridges (raffleizeBridge)

myTypes :: [SumType 'Haskell]
myTypes =
  [ let p = (Proxy :: Proxy RaffleConfig) in equal p (mkSumType p),
    let p = (Proxy :: Proxy RaffleParam) in equal p (mkSumType p),
    let p = (Proxy :: Proxy RaffleStateData) in equal p (mkSumType p),
    let p = (Proxy :: Proxy TicketStateData) in equal p (mkSumType p),
    let p = (Proxy :: Proxy UserAction) in equal p (mkSumType p),
    let p = (Proxy :: Proxy TicketOwnerAction) in equal p (mkSumType p),
    let p = (Proxy :: Proxy RaffleOwnerAction) in equal p (mkSumType p),
    let p = (Proxy :: Proxy RaffleizeAction) in equal p (mkSumType p),
    let p = (Proxy :: Proxy AdminAction) in equal p (mkSumType p),
    let p = (Proxy :: Proxy RaffleInfo) in equal p (mkSumType p),
    let p = (Proxy :: Proxy TicketInfo) in equal p (mkSumType p),
    mkSumType (Proxy :: Proxy UserAddresses),
    mkSumType (Proxy :: Proxy RaffleizeInteraction),
    mkSumType (Proxy :: Proxy AddWitAndSubmitParams)
  ]

main :: IO ()
main = do
  writePSTypes purescriptProjectSrcPath (buildBridge raffleizeBridge) myTypes
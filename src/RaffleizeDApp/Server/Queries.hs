module RaffleizeDApp.Server.Queries where

import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups (lookupActiveRaffles)

handleLookup :: IO String
handleLookup = return "hello"

handleGetRaffles :: ProviderCtx -> IO [RaffleInfo]
handleGetRaffles pCtx = runQuery pCtx lookupActiveRaffles

handleGetOneRaffle :: ProviderCtx -> IO RaffleInfo
handleGetOneRaffle pCtx = head <$> handleGetRaffles pCtx

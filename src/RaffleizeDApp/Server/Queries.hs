module RaffleizeDApp.Server.Queries where

import PlutusLedgerApi.V1 qualified
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Transactions

handleLookup :: IO String
handleLookup = return "hello"

handleGetRaffles :: IO [(RaffleStateData, PlutusLedgerApi.V1.Value)]
handleGetRaffles = runContextWithCfgProviders "get raffles" queryRaffles

handleGetInfo :: IO RaffleInfo
handleGetInfo = head <$> runContextWithCfgProviders "get raffles infos" queryRafflesInfos

handleGetRaffle :: IO RaffleStateData
handleGetRaffle = fst . head <$> runContextWithCfgProviders "get raffles" queryRaffles

handleGetValue :: IO PlutusLedgerApi.V1.Value
handleGetValue = snd . head <$> runContextWithCfgProviders "get raffles" queryRaffles

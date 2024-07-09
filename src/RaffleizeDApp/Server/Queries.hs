module RaffleizeDApp.Server.Queries where

import GeniusYield.Types (GYAddress)
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups (lookupActiveRaffles, lookupTicketsOfAddress)

handleLookup :: IO String
handleLookup = return "hello"

handleGetRaffles :: ProviderCtx -> IO [RaffleInfo]
handleGetRaffles pCtx = runQuery pCtx lookupActiveRaffles

handleGetOneRaffle :: ProviderCtx -> IO RaffleInfo
handleGetOneRaffle pCtx = head <$> handleGetRaffles pCtx

handleGetMyTickets :: ProviderCtx -> GYAddress -> IO [TicketInfo]
handleGetMyTickets pCtx addr = runQuery pCtx (lookupTicketsOfAddress addr)
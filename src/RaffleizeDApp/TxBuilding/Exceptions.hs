module RaffleizeDApp.TxBuilding.Exceptions where

import Control.Exception (Exception)
import GeniusYield.HTTP.Errors (IsGYApiError)

data MissingContextNFT = MissingContextNFT deriving (Show, Typeable)
instance Exception MissingContextNFT
instance IsGYApiError MissingContextNFT

data RaffleizeDatumNotFound = RaffleizeDatumNotFound deriving (Show, Typeable)
instance Exception RaffleizeDatumNotFound
instance IsGYApiError RaffleizeDatumNotFound

data TicketDatumNotFound = TicketDatumNotFound deriving (Show, Typeable)
instance Exception TicketDatumNotFound
instance IsGYApiError TicketDatumNotFound

data InlineDatumNotFound = InlineDatumNotFound deriving (Show, Typeable)
instance Exception InlineDatumNotFound
instance IsGYApiError InlineDatumNotFound

data TooManyUTxOs = TooManyUTxOs deriving (Show, Typeable)
instance Exception TooManyUTxOs
instance IsGYApiError TooManyUTxOs

data InvalidRaffleState = InvalidRaffleState deriving (Show, Typeable)
instance Exception InvalidRaffleState
instance IsGYApiError InvalidRaffleState
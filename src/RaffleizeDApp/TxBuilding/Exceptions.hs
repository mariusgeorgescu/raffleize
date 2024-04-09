module RaffleizeDApp.TxBuilding.Exceptions where

import Control.Exception (Exception)
import GeniusYield.HTTP.Errors (IsGYApiError)

data MissingContextNFT = MissingContextNFT deriving (Show, Typeable)
instance Exception MissingContextNFT
instance IsGYApiError MissingContextNFT
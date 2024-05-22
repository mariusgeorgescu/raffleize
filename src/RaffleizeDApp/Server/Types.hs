module RaffleizeDApp.Server.Types where

import PlutusLedgerApi.V3 (Value)
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleStateData, RaffleizeActionLabel)

data RaffleInfo = RaffleInfo
  { riRsd :: RaffleStateData
  , riVlaue :: Value
  , riImage :: String
  , riStateLabel :: String
  , riAvailableActions :: [RaffleizeActionLabel]
  }
  deriving (Generic, ToJSON, FromJSON)
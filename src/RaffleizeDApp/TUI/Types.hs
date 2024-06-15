module RaffleizeDApp.TUI.Types where

import Brick.Widgets.List

------------------------------------------------------------------------------------------------

-- *   Types

------------------------------------------------------------------------------------------------
type RaffleizeEvent = ()

data NameResources
  = CommitDdlField
  | RevealDdlField
  | MinNoTokensField
  | AddToValueListField
  | AddToValueAmountField
  | TicketPriceField
  | ActiveRafflesListField
  | MyRafflesListField
  | MyTicketsListField
  | ValueItemsList
  | AvailableValueItemsList
  | ConstructedValueItemsList
  | ValueItemsViewPort
  | TokenNameField
  | MintAmountField
  | SecretField
  | RevealedSecretField
  | MintTokensAddressField
  | SendTicketAddressField
  | SendRaffleAddressField
  | Other
  deriving (Eq, Ord, Show, Generic)

data Screen
  = MainScreen
  | CreateRaffleScreen
  | MintTokenScreen
  | ActiveRafflesScreen
  | MyRafflesScreen
  | BuyTicketScreen
  | MyTicketsScreen
  | ConstructValueScreen
  | RevealTicketSecretScreen
  deriving (Eq, Ord, Enum, Show)

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

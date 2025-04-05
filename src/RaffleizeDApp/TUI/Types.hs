{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

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
  | MnemonicField
  | Other
  deriving (Eq, Ord, Show, Generic)

data Screen
  = MainScreen
  | CreateRaffleScreen
  | UpdateRaffleScreen
  | MintTokenScreen
  | ActiveRafflesScreen
  | MyRafflesScreen
  | BuyTicketScreen
  | MyTicketsScreen
  | ConstructValueScreen
  | RevealTicketSecretScreen
  | ImportWalletFromMnemonic
  deriving (Eq, Ord, Enum, Show)

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

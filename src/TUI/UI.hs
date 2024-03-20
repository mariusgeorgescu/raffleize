{-# LANGUAGE OverloadedStrings #-}

module TUI.UI where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty.Input.Events

import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.Types (GYPaymentSigningKey)

import Brick.Widgets.Core (str, vBox)

import Graphics.Vty (defAttr)
import RaffleizeDApp.Constants (raffleizeLogoPath)
import RaffleizeDApp.TxBuilding.Transactions
import System.IO.Extra (readFile)

-- Types

data RaffleizeUI = RaffleizeUI
  { atlasCoreConfig :: Maybe GYCoreConfig
  , raffleizeTxBuildingContext :: Maybe RaffleizeTxBuildingContext
  , operationSkeyFile :: Maybe GYPaymentSigningKey
  , logo :: String
  }
  deriving (Show)

data RaffleizeEvent = RaffleizeEvent

{- | Named resources

Not currently used, but will be easier to refactor
if we call this "Name" now.
-}
type Name = ()

-- App definition

app :: App RaffleizeUI RaffleizeEvent Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap defAttr []
    }

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain app initialState
  print endState

-- Building the initial state

buildInitialState :: IO RaffleizeUI
buildInitialState = do
  s <- readFile raffleizeLogoPath
  pure (RaffleizeUI Nothing Nothing Nothing s)

-- Handling events

handleEvent :: RaffleizeUI -> BrickEvent Name RaffleizeEvent -> EventM Name (Next RaffleizeUI)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    _ -> continue s
  _ -> continue s

-- Drawing

drawUI :: RaffleizeUI -> [Widget Name]
drawUI s = [vBox [str (logo s), str "TODO"]]

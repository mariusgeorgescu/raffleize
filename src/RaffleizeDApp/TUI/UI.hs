{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module RaffleizeDApp.TUI.UI where

import Brick.AttrMap
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Table
import Control.Lens
import Control.Monad.IO.Class
import Data.List qualified
import Data.Text qualified
import Data.Vector qualified
import GeniusYield.GYConfig
import GeniusYield.Types
import Graphics.Vty
import PlutusLedgerApi.V1.Value
import PlutusPrelude (showText)
import RaffleizeDApp.Constants
import RaffleizeDApp.TUI.Actions
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)

------------------------------------------------------------------------------------------------

-- *   Types

------------------------------------------------------------------------------------------------

data NameResources
  = CommitDDL
  | RevealDDL
  | MinNoTokens
  | StakeValue
  | StakeAmount
  | ValueItemsList
  | ValueItemsViewPort
  | TokenNameField
  | MintAmountField
  | Other
  deriving (Eq, Ord, Show, Generic)

------------------------------------------------------------------------------------------------

-- **  Mint test tokens form

------------------------------------------------------------------------------------------------

data MintTokenFormState = MintTokenFormState
  { _tokenNameField :: Text
  , _mintAmount :: Int
  }
  deriving (Show)

makeLenses ''MintTokenFormState

mkMintTokenForm :: MintTokenFormState -> Form MintTokenFormState e NameResources
mkMintTokenForm =
  newForm
    [ (txt "Token name: " <+>) @@= editTextField tokenNameField TokenNameField (Just 1)
    , (txt "Minting amount: " <+>) @@= editShowableField mintAmount MintAmountField
    ]

------------------------------------------------------------------------------------------------

-- **  Create new raffle form

------------------------------------------------------------------------------------------------

data CreateRaffleFormState = CreateRaffleFormState
  { _commitDdl :: Text
  , _revealDdl :: Text
  , _minNoTickets :: Int
  , _availableAssets :: Data.Vector.Vector (CurrencySymbol, TokenName, Integer)
  , _selectedAsset :: Maybe (CurrencySymbol, TokenName, Integer)
  , _amount :: Int
  }
  deriving (Show)

makeLenses ''CreateRaffleFormState

mkCreateRaffleForm :: CreateRaffleFormState -> Form CreateRaffleFormState e NameResources
mkCreateRaffleForm =
  newForm
    [ (txt "Commit deadline: " <+>) @@= editTextField commitDdl CommitDDL (Just 1)
    , (txt "Reveal deadline: " <+>) @@= editTextField revealDdl RevealDDL (Just 1)
    , (txt "Min. no. of tickets: " <+>) @@= editShowableField minNoTickets MinNoTokens
    , (txt "Select Asset" <+>) @@= listField _availableAssets selectedAsset (valueItemWidget True) 5 StakeValue
    , (txt "Amount: " <+>) @@= editShowableField amount StakeAmount
    ]

------------------------------------------------------------------------------------------------

-- *   Main State

------------------------------------------------------------------------------------------------

data Screen
  = MainScreen
  | CreateRaffleScreen
  | MintTokenScreen
  deriving (Eq, Ord, Enum, Show)

type RaffleizeEvent = ()

data RaffleizeUI = RaffleizeUI
  { atlasConfig :: Maybe GYCoreConfig
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , adminSkey :: Maybe GYPaymentSigningKey
  , connectedWalletAddr :: Maybe GYAddress
  , adminBalance :: Maybe Value
  , logo :: String
  , message :: Text
  , mintTokenForm :: Form MintTokenFormState RaffleizeEvent NameResources
  , createRaffleForm :: Form CreateRaffleFormState RaffleizeEvent NameResources
  , currentScreen :: Screen
  }

------------------------------------------------------------------------------------------------

-- *   App definition

------------------------------------------------------------------------------------------------

app :: App RaffleizeUI RaffleizeEvent NameResources
app =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return $ liftIO buildInitialState
    , appAttrMap = const theMap
    }

tui :: IO ()
tui = do
  initialState <- buildInitialState
  _endState <- defaultMain app initialState
  return ()

------------------------------------------------------------------------------------------------

-- *  Building the initial state

------------------------------------------------------------------------------------------------
buildInitialState :: IO RaffleizeUI
buildInitialState = do
  logo <- readFile raffleizeLogoPath
  atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
  validatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  skey <- readPaymentKeyFile operationSkeyFilePath
  let mintTokenForm = mkMintTokenForm (MintTokenFormState "test-tokens" 1)
  case skey of
    Nothing -> do
      let createRaffleForm = mkCreateRaffleForm (CreateRaffleFormState mempty mempty 0 mempty Nothing 0)
      return (RaffleizeUI atlasConfig validatorsConfig skey Nothing Nothing logo mempty mintTokenForm createRaffleForm MainScreen)
    Just skey' -> do
      (addr, val) <- liftIO $ getAddressAndValue skey'
      let createRaffleForm = mkCreateRaffleForm (CreateRaffleFormState mempty mempty 0 (Data.Vector.fromList (flattenValue val)) Nothing 0)
      return (RaffleizeUI atlasConfig validatorsConfig skey (Just addr) (Just val) logo mempty mintTokenForm createRaffleForm MainScreen)

------------------------------------------------------------------------------------------------

-- *  Handling Events

------------------------------------------------------------------------------------------------

handleEvent :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources (Next RaffleizeUI)
handleEvent s e =
  case e of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> continue s {message = "", currentScreen = MainScreen}
      EvKey key _modifiers ->
        case currentScreen s of
          CreateRaffleScreen ->
            let f = createRaffleForm s
                invalid_fields = invalidFields f
             in case key of
                  KEnter ->
                    do
                      if null invalid_fields
                        then do
                          liftIO clearScreen
                          txOutRef <- liftIO $ createRaffle (fromJust (adminSkey s))
                          let nid = (cfgNetworkId . fromJust . atlasConfig) s
                          continue s {message = "RAFFLE SUCCESFULLY CREATED !\n" <> showLink nid "tx" txOutRef}
                        else continue s
                  _ -> do
                    updated_form <- handleFormEvent e f

                    let updated_form2 =
                          if key `elem` [KUp, KDown]
                            then do
                              let updated_form_state = formState updated_form
                              let currentAmount = _amount updated_form_state
                              let selectedElementAmount = maybe (0 :: Int) (\(_, _, i) -> fromIntegral i) (_selectedAsset updated_form_state)
                              let updated_form_state_again = updated_form_state {_amount = selectedElementAmount}
                              if selectedElementAmount /= currentAmount then updateFormState updated_form_state_again updated_form else updated_form
                            else updated_form
                    let fieldValidations = [setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (_commitDdl (formState updated_form2)))) CommitDDL]
                    let validated_form = foldr' ($) updated_form2 fieldValidations
                    continue s {createRaffleForm = validated_form}
          MintTokenScreen ->
            let mtForm = mintTokenForm s
                mtFormState = formState mtForm
                mtTokenNameField = mtFormState ^. tokenNameField
                mtAmountField = mtFormState ^. mintAmount
                invalid_fields = invalidFields mtForm
             in case key of
                  KEnter ->
                    do
                      if null invalid_fields
                        then do
                          liftIO clearScreen
                          txOutRef <-
                            liftIO $
                              mintTestTokens
                                (fromJust (adminSkey s))
                                (Data.Text.unpack mtTokenNameField)
                                (fromIntegral mtAmountField)
                          let nid = (cfgNetworkId . fromJust . atlasConfig) s
                          continue
                            s
                              { message = "TEST TOKENS SUCCESFULLY MINTED !\n" <> showLink nid "tx" txOutRef
                              , currentScreen = MainScreen
                              }
                        else continue s
                  _ -> do
                    updated_form <- handleFormEvent e mtForm
                    let newMtFormState = formState mtForm
                    let newMtTokenNameField = newMtFormState ^. tokenNameField
                    let newMtAmountField = newMtFormState ^. mintAmount
                    let fieldValidations =
                          [ setFieldValid (Data.Text.length newMtTokenNameField <= tokenNameMaxLength) TokenNameField
                          , setFieldValid (newMtAmountField > 0) MintAmountField
                          ]
                    let validated_form = foldr' ($) updated_form fieldValidations
                    continue s {mintTokenForm = validated_form}
          MainScreen -> case key of
            KDown -> do
              let vscroll = viewportScroll ValueItemsViewPort
              vScrollBy vscroll 1
              continue s
            KUp -> do
              let vscroll = viewportScroll ValueItemsViewPort
              vScrollBy vscroll (-1)
              continue s
            (KChar c) -> case c of
              'q' -> halt s
              'r' -> continue s {message = "Refresh Screen"}
              'g' -> do
                liftIO $ generateNewAdminSkey operationSkeyFilePath
                s' <- liftIO $ updateFromConfigFiles s
                continue s'
              'e' -> do
                liftIO $ sequence_ [exportRaffleScript, exportTicketScript, exportMintingPolicy]
                continue s {message = "VALIDATORS SUCCESFULLY EXPORTED !\n" <> Data.Text.pack (Data.List.intercalate "\n" [raffleizeValidatorFile, ticketValidatorFile, mintingPolicyFile])}
              _ ->
                if isJust (adminSkey s)
                  then do
                    case c of
                      'm' -> continue s {currentScreen = MintTokenScreen}
                      'c' -> do
                        continue s {currentScreen = CreateRaffleScreen}
                      'l' -> do
                        let skey = fromMaybe (error "No skey") $ adminSkey s
                        (addr, val) <- liftIO $ getAddressAndValue skey
                        let createRaffleForm_state = formState (createRaffleForm s)
                        let newCreateRaffleForm = mkCreateRaffleForm (createRaffleForm_state {_availableAssets = Data.Vector.fromList (flattenValue val)})
                        continue s {connectedWalletAddr = Just addr, adminBalance = Just val, createRaffleForm = newCreateRaffleForm}
                      'd' -> do
                        liftIO clearScreen
                        liftIO deployValidators
                        s' <- liftIO $ updateFromConfigFiles s
                        continue s' {message = "VALIDATORS SUCCESFULLY DEPLOYED !\nTxOuts references are saved to " <> showText raffleizeValidatorsConfig}
                      _ -> continue s
                  else continue s
            _ -> continue s
      _ -> continue s
    _ -> continue s

updateFromConfigFiles :: RaffleizeUI -> IO RaffleizeUI
updateFromConfigFiles s = do
  skey <- readPaymentKeyFile operationSkeyFilePath
  atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
  validatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  return $ s {adminSkey = skey, atlasConfig = atlasConfig, validatorsConfig = validatorsConfig}

------------------------------------------------------------------------------------------------

-- *  Drawing

------------------------------------------------------------------------------------------------

drawUI :: RaffleizeUI -> [Widget NameResources]
drawUI s =
  joinBorders . withBorderStyle unicode . borderWithLabel (txt " RAFFLEIZE - C.A.R.D.A.N.A ")
    <$> [ if Data.Text.null (message s) then emptyWidget else center (withAttr "highlight" $ txt (message s)) <=> txt "[ESC] - Close"
        , if currentScreen s == MintTokenScreen then mintTestTokensScreen s else emptyWidget
        , if currentScreen s == CreateRaffleScreen then createRaffleScreen s else emptyWidget
        , mainScreen s
        ]

------------------------------------------------------------------------------------------------

-- **  Main Screen

------------------------------------------------------------------------------------------------

mainScreen :: RaffleizeUI -> Widget NameResources
mainScreen s =
  vBox
    [ hCenter $ withAttr "highlight" $ str (logo s)
    , hBorder
    , center $ bodyWidget s
    ]

bodyWidget :: RaffleizeUI -> Widget NameResources
bodyWidget s =
  vBox $
    padLeftRight 1
      <$> [ summaryWidget s
          , hBorder
          , assetsWidget (cfgNetworkId <$> atlasConfig s) (adminSkey s) (connectedWalletAddr s) (adminBalance s)
          ]

summaryWidget :: RaffleizeUI -> Widget NameResources
summaryWidget s =
  hCenter $
    hBox
      [ availableActionsWidget s
      , providersWidget (atlasConfig s)
      , validatorsWidget (cfgNetworkId <$> atlasConfig s) (validatorsConfig s)
      , walletFlagWidget (adminSkey s)
      ]

symbolWidget :: Bool -> Widget n
symbolWidget v = if v then withAttr "good" $ txt "✔" else withAttr "warning" $ txt "X"

validatorsWidget :: Maybe GYNetworkId -> Maybe RaffleizeTxBuildingContext -> Widget n
validatorsWidget mnid mv =
  borderWithLabel (txt "VALIDATORS") $
    renderTable $
      table $
        [txt "Deployed: ", symbolWidget (isJust mv)] : case (mnid, mv) of
          (Just nid, Just (RaffleizeTxBuildingContext {..})) ->
            [ [txt "Raffle Validator", txOutRefWidget nid raffleValidatorRef]
            , [txt "Ticket Validator", txOutRefWidget nid ticketValidatorRef]
            ]
          _ -> []

walletFlagWidget :: Maybe a -> Widget NameResources
walletFlagWidget mkey =
  borderWithLabel (txt "WALLET") $
    renderTable $
      table
        [[txt "Connected ", symbolWidget (isJust mkey)]]

txOutRefWidget :: GYNetworkId -> GYTxOutRef -> Widget n
txOutRefWidget nid t =
  let txoutrefText = showTxOutRef t
   in hyperlink (showLink nid "tx" txoutrefText) $ withAttr "good" $ txt txoutrefText

providersWidget :: Maybe GYCoreConfig -> Widget n
providersWidget mp =
  borderWithLabel (txt "BLOCKCHAIN PROVIDER") $
    renderTable $
      table $
        [ txt "Loaded: "
        , symbolWidget (isJust mp)
        ]
          : case mp of
            Nothing -> []
            Just cfg ->
              [ [txt "Provider: ", printProvider cfg]
              , [txt "Network: ", printNetwork cfg]
              ]

printProvider :: GYCoreConfig -> Widget n
printProvider cfg = withAttr "good" . txt $ case cfgCoreProvider cfg of
  (GYNodeKupo {}) -> "KUPO"
  (GYMaestro {}) -> "MAESTRO"
  (GYBlockfrost {}) -> "BLOCKFROST"

printNetwork :: GYCoreConfig -> Widget n
printNetwork cfg = withAttr "good" . txt $ case cfgNetworkId cfg of
  GYMainnet -> "MAINNET"
  GYTestnetPreprod -> "PREPROD"
  GYTestnetPreview -> "PREVIEW"
  GYTestnetLegacy -> "TESTNET-LEGACY"
  GYPrivnet -> "PRIVNET"

adaBalanceWidget :: Value -> Widget n
adaBalanceWidget val = withAttr "good" $ txt (showText (fromValue val) <> "\n")

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

assetsBalanceWidget :: Value -> Widget NameResources
assetsBalanceWidget val =
  let
    valueItemsList = Brick.Widgets.List.list ValueItemsList (flattenValue val) 5
   in
    borderWithLabel (txt "ASSETS") $
      visible $
        withVScrollBarHandles $
          withVScrollBars OnRight $
            viewport ValueItemsViewPort Vertical $
              vLimit 300 $
                hLimit 110 $
                  renderList (valueItemWidget False) False valueItemsList

assetClassItemWidget :: CurrencySymbol -> TokenName -> Widget n
assetClassItemWidget cs tn = str (show cs) <=> hBorder <=> str (toString tn)

valueItemWidget :: Bool -> Bool -> (CurrencySymbol, TokenName, Integer) -> Widget n
valueItemWidget selectable hasFocus (cs, tn, i) = (if hasFocus && selectable then withAttr "selected" else id) $ vLimit 5 $ hLimit 100 (border (assetClassItemWidget cs tn <+> vBorder <+> center (str (show i))))

addressWidget :: GYNetworkId -> GYAddress -> Widget n
addressWidget nid addr =
  let addrText = addressToText addr
   in hyperlink (showLink nid "address" addrText) $ withAttr "good" $ txt addrText

assetsWidget :: Maybe GYNetworkId -> Maybe a -> Maybe GYAddress -> Maybe Value -> Widget NameResources
assetsWidget (Just nid) (Just _key) (Just addr) (Just val) =
  borderWithLabel (txt "WALLET") $
    hBox
      [ assetsAdaWidget nid addr val
      , vBorder
      , assetsBalanceWidget val
      ]
assetsWidget _ _ _ _ = emptyWidget

assetsAdaWidget :: GYNetworkId -> GYAddress -> Value -> Widget NameResources
assetsAdaWidget nid addr val =
  renderTable $
    table
      [ [txt "Address: ", addressWidget nid addr]
      , [txt "Ada | ₳ |:", adaBalanceWidget val]
      ]

availableActionsWidget :: RaffleizeUI -> Widget n
availableActionsWidget s =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      ( txt
          <$> filter
            (not . Data.Text.null)
            ( ( if isNothing . adminSkey $ s
                  then ["[G] - Generate new admin skey"]
                  else ["[D] - Deploy Raffleize Validators", "[L] - Get wallet balance", "[C] - Create raffle", "[M] - Mint some test tokens"]
              )
                ++ [ "[R] - Refresh screen"
                   , "[E] - Export validators"
                   , "[Q] - Quit"
                   ]
            )
      )

------------------------------------------------------------------------------------------------

-- **  Create Raffle Screen

------------------------------------------------------------------------------------------------

createRaffleScreen :: RaffleizeUI -> Widget NameResources
createRaffleScreen s =
  let ivfs = invalidFields (createRaffleForm s)
   in center $
        borderWithLabel (txt " CREATE A NEW RAFFLE ") $
          vBox $
            padAll 1
              <$> [ withAttr formAttr (withAttr invalidFormInputAttr (withAttr focusedFormInputAttr (renderForm $ createRaffleForm s)))
                  , hBorder
                  , hCenter $ txt (printMTivfs ivfs)
                  , hBorder
                  , hCenter $ formActionsWidget (null ivfs) "Create raffle"
                  ]

------------------------------------------------------------------------------------------------

-- **  Mint Test Tokens Screen

------------------------------------------------------------------------------------------------

mintTestTokensScreen :: RaffleizeUI -> Widget NameResources
mintTestTokensScreen s =
  let ivfs = invalidFields (mintTokenForm s)
   in center $
        borderWithLabel (txt " MINT TEST TOKENS ") $
          vBox $
            padAll 1
              <$> [ withAttr formAttr (withAttr invalidFormInputAttr (withAttr focusedFormInputAttr (renderForm $ mintTokenForm s)))
                  , hBorder
                  , hCenter $ withAttr "warning" $ txt (printMTivfs ivfs)
                  , hBorder
                  , hCenter $ formActionsWidget (null ivfs) "Mint tokens"
                  ]

formActionsWidget :: Bool -> Text -> Widget n
formActionsWidget isValid s =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      [ txt "[ESC]   - Close          "
      , if isValid then txt ("[Enter] - " <> s) else emptyWidget
      ]

printMTivf :: NameResources -> Text
printMTivf TokenNameField = "Token name must have maximum " <> showText tokenNameMaxLength <> " characters!"
printMTivf MintAmountField = "The minting amount must be a natural number!"
printMTivf _ = ""

printMTivfs :: [NameResources] -> Text
printMTivfs x = Data.Text.intercalate "\n" $ printMTivf <$> x

tokenNameMaxLength :: Int
tokenNameMaxLength = 32

------------------------------------------------------------------------------------------------

-- **  Styling

------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    (white `on` black)
    [ ("highlight", fg magenta)
    , ("warning", fg red)
    , ("good", fg green)
    , ("action", fg yellow)
    , ("selected", bg blue)
    , (focusedFormInputAttr, fg brightBlue)
    , (invalidFormInputAttr, white `on` red)
    ]

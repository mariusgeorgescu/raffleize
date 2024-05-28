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
import RaffleizeDApp.Constants
import RaffleizeDApp.TUI.Actions
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)

-----

-------

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

data MintTokenFormState = MintTokenFormState
  { _tokenNameField :: Text
  , _mintAmount :: Int
  }
  deriving (Show)

makeLenses ''MintTokenFormState

mkMintTokenForm :: MintTokenFormState -> Form MintTokenFormState e NameResources
mkMintTokenForm =
  newForm
    [ (str "Token name: " <+>) @@= editTextField tokenNameField TokenNameField (Just 1)
    , (str "Minting amount: " <+>) @@= editShowableField mintAmount MintAmountField
    ]

------------------------------------------------------------------------------------------------

data CreateRaffleFormState = CreateRaffleFormState
  { _commitDdl :: String
  , _revealDdl :: String
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
    [ (str "Commit deadline: " <+>) @@= editShowableField commitDdl CommitDDL
    , (str "Reveal deadline: " <+>) @@= editShowableField revealDdl RevealDDL
    , (str "Min. no. of tickets: " <+>) @@= editShowableField minNoTickets MinNoTokens
    , (str "Select Asset" <+>) @@= listField _availableAssets selectedAsset (valueItemWidget True) 5 StakeValue
    , (str "Amount: " <+>) @@= editShowableField amount StakeAmount
    ]

------------------------------------------------------------------------------------------------

-- *   Types

------------------------------------------------------------------------------------------------

data RaffleizeUI = RaffleizeUI
  { atlasConfig :: Maybe GYCoreConfig
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , adminSkey :: Maybe GYPaymentSigningKey
  , connectedWalletAddr :: Maybe GYAddress
  , adminBalance :: Maybe Value
  , logo :: String
  , message :: String
  , mintTokenForm :: Form MintTokenFormState RaffleizeEvent NameResources
  , createRaffleForm :: Form CreateRaffleFormState RaffleizeEvent NameResources
  , currentScreen :: Screen
  }

data Screen = MainScreen | CreateRaffleScreen | MintTokenScreen deriving (Eq, Ord, Enum, Show)

data RaffleizeEvent = RaffleizeEvent

------------------------------------------------------------------------------------------------

-- *   App definition

------------------------------------------------------------------------------------------------

app :: App RaffleizeUI RaffleizeEvent NameResources
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
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
  let mintTokenForm = mkMintTokenForm (MintTokenFormState "test-tokens" 0)
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

                    let updated_form' =
                          if key `elem` [KUp, KDown]
                            then do
                              let updated_form_state = formState updated_form
                              let currentAmount = _amount updated_form_state
                              let selectedElementAmount = maybe (0 :: Int) (\(_, _, i) -> fromIntegral i) (_selectedAsset updated_form_state)
                              let updated_form_state_again = updated_form_state {_amount = selectedElementAmount}
                              if selectedElementAmount /= currentAmount then updateFormState updated_form_state_again updated_form else updated_form
                            else updated_form
                    let fieldValidations = []
                    let validated_form = foldr' ($) updated_form' fieldValidations
                    continue s {createRaffleForm = validated_form}
          MintTokenScreen ->
            let f = mintTokenForm s
                mintTokensForm_state = formState f
                invalid_fields = invalidFields f
             in case key of
                  KEnter ->
                    do
                      if null invalid_fields
                        then do
                          liftIO clearScreen
                          let tn = _tokenNameField mintTokensForm_state
                          let minamnt = _mintAmount mintTokensForm_state
                          txOutRef <- liftIO $ mintTestTokens (fromJust (adminSkey s)) (Data.Text.unpack tn) (fromIntegral minamnt)
                          let nid = (cfgNetworkId . fromJust . atlasConfig) s
                          continue
                            s
                              { message = "TEST TOKENS SUCCESFULLY MINTED !\n" <> showLink nid "tx" txOutRef
                              , currentScreen = MainScreen
                              }
                        else continue s
                  _ -> do
                    updated_form <- handleFormEvent e f
                    let tnfield = _tokenNameField (formState updated_form)
                    let fieldValidations = [setFieldValid (Data.Text.length tnfield <= tokenNameMaxLength) TokenNameField]
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
                continue s {message = "VALIDATORS SUCCESFULLY EXPORTED !\n" ++ Data.List.intercalate "\n" [raffleizeValidatorFile, ticketValidatorFile, mintingPolicyFile]}
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
                        continue s' {message = "VALIDATORS SUCCESFULLY DEPLOYED !\nTxOuts references are saved to " ++ show raffleizeValidatorsConfig}
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

theMap :: AttrMap
theMap =
  attrMap
    (white `on` black)
    [ ("highlight", fg magenta)
    , ("warning", fg red)
    , ("good", fg green)
    , ("action", fg yellow)
    , ("selected", bg blue)
    , (focusedFormInputAttr, fg blue)
    , (invalidFormInputAttr, white `on` red)
    ]

drawUI :: RaffleizeUI -> [Widget NameResources]
drawUI s =
  joinBorders . withBorderStyle unicode . borderWithLabel (str "RAFFLEIZE - C.A.R.D.A.N.A")
    <$> [ if null (message s) then emptyWidget else center (withAttr "highlight" $ str (message s)) <=> str "[ESC] - Close"
        , if currentScreen s == MintTokenScreen then mintTestTokensScreen s else emptyWidget
        , if currentScreen s == CreateRaffleScreen then createRaffleScreen s else emptyWidget
        , mainMenu s
        ]

createRaffleScreen :: RaffleizeUI -> Widget NameResources
createRaffleScreen s =
  let ivfs = invalidFields (createRaffleForm s)
   in center $
        borderWithLabel (str "CREATE A NEW RAFFLE") $
          vBox $
            padAll 1
              <$> [ withAttr formAttr (withAttr invalidFormInputAttr (withAttr focusedFormInputAttr (renderForm $ createRaffleForm s)))
                  , hBorder
                  , hCenter $ str (printMTivfs ivfs)
                  , hBorder
                  , hCenter $ formActionsWidget (null ivfs) "Create raffle"
                  ]

mintTestTokensScreen :: RaffleizeUI -> Widget NameResources
mintTestTokensScreen s =
  let ivfs = invalidFields (mintTokenForm s)
   in center $
        borderWithLabel (str "MINT TEST TOKENS") $
          vBox $
            padAll 1
              <$> [ withAttr formAttr (withAttr invalidFormInputAttr (withAttr focusedFormInputAttr (renderForm $ mintTokenForm s)))
                  , hBorder
                  , hCenter $ str (printMTivfs ivfs)
                  , hBorder
                  , hCenter $ formActionsWidget (null ivfs) "Mint tokens"
                  ]

formActionsWidget :: Bool -> String -> Widget n
formActionsWidget isValid s =
  withAttr "action" . borderWithLabel (str "AVAILABLE ACTIONS") $
    vBox
      [ str "[ESC]   - Close          "
      , if isValid then str ("[Enter] - " <> s) else emptyWidget
      ]

printMTivf :: NameResources -> String
printMTivf TokenNameField = "Token name must have maximum " <> show tokenNameMaxLength <> " characters!"
printMTivf MintAmountField = "The minting amount must be an integer value !"
printMTivf _ = ""

printMTivfs :: [NameResources] -> String
printMTivfs x = Data.List.intercalate "\n" $ printMTivf <$> x

tokenNameMaxLength :: Int
tokenNameMaxLength = 32

------------------------------------------------------------------------------------------------

-- **  Widgets

------------------------------------------------------------------------------------------------

mainMenu :: RaffleizeUI -> Widget NameResources
mainMenu s =
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
symbolWidget v = if v then withAttr "good" $ str "✔" else withAttr "warning" $ str "X"

validatorsWidget :: Maybe GYNetworkId -> Maybe RaffleizeTxBuildingContext -> Widget n
validatorsWidget mnid mv =
  borderWithLabel (str "VALIDATORS") $
    renderTable $
      table $
        [str "Deployed: ", symbolWidget (isJust mv)] : case (mnid, mv) of
          (Just nid, Just (RaffleizeTxBuildingContext {..})) ->
            [ [str "Raffle Validator", txOutRefWidget nid raffleValidatorRef]
            , [str "Ticket Validator", txOutRefWidget nid ticketValidatorRef]
            ]
          _ -> []

walletFlagWidget :: Maybe a -> Widget NameResources
walletFlagWidget mkey =
  borderWithLabel (str "WALLET") $
    renderTable $
      table
        [[str "Connected ", symbolWidget (isJust mkey)]]

txOutRefWidget :: GYNetworkId -> GYTxOutRef -> Widget n
txOutRefWidget nid t = withAttr "good" $ txt $ showTxOutRef t

providersWidget :: Maybe GYCoreConfig -> Widget n
providersWidget mp =
  borderWithLabel (str "BLOCKCHAIN PROVIDER") $
    renderTable $
      table $
        [ str "Loaded: "
        , symbolWidget (isJust mp)
        ]
          : case mp of
            Nothing -> []
            Just cfg ->
              [ [str "Provider: ", printProvider cfg]
              , [str "Network: ", printNetwork cfg]
              ]

printProvider :: GYCoreConfig -> Widget n
printProvider cfg = withAttr "good" . str $ case cfgCoreProvider cfg of
  (GYNodeKupo {}) -> "KUPO"
  (GYMaestro {}) -> "MAESTRO"
  (GYBlockfrost {}) -> "BLOCKFROST"

printNetwork :: GYCoreConfig -> Widget n
printNetwork cfg = withAttr "good" . str $ case cfgNetworkId cfg of
  GYMainnet -> "MAINNET"
  GYTestnetPreprod -> "PREPROD"
  GYTestnetPreview -> "PREVIEW"
  GYTestnetLegacy -> "TESTNET-LEGACY"
  GYPrivnet -> "PRIVNET"

adaBalanceWidget :: Value -> Widget n
adaBalanceWidget val = withAttr "good" $ str (show (fromValue val) ++ "\n")

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

assetsBalanceWidget :: Value -> Widget NameResources
assetsBalanceWidget val =
  let
    valueItemsList = Brick.Widgets.List.list ValueItemsList (flattenValue val) 5
   in
    borderWithLabel (str "ASSETS") $
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
addressWidget nid addr = withAttr "good" $ txt $ addressToText addr

assetsWidget :: Maybe GYNetworkId -> Maybe a -> Maybe GYAddress -> Maybe Value -> Widget NameResources
assetsWidget (Just nid) (Just _key) (Just addr) (Just val) =
  borderWithLabel (str "WALLET") $
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
      [ [str "Address: ", addressWidget nid addr]
      , [str "Ada | ₳ |:", adaBalanceWidget val]
      ]

availableActionsWidget :: RaffleizeUI -> Widget n
availableActionsWidget s =
  withAttr "action" . borderWithLabel (str "AVAILABLE ACTIONS") $
    vBox
      ( str
          <$> filter
            (not . null)
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

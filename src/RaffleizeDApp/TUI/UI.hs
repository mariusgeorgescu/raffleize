module RaffleizeDApp.TUI.UI where

import Brick.AttrMap
import Brick.Focus (focusGetCurrent, focusNext)
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Table (renderTable, table)
import Codec.Serialise.Internal.GeneralisedUTF8 ()
import Control.Lens
import Control.Monad.IO.Class
import Data.List qualified
import Data.Maybe (fromMaybe)
import Data.Text qualified
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified
import GHC.Real
import GeniusYield.GYConfig
import GeniusYield.Types
import Graphics.Vty
import PlutusLedgerApi.V1.Value
import PlutusPrelude (showText)
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.TUI.Actions
import RaffleizeDApp.TUI.RaffleizeWidgets
import RaffleizeDApp.TUI.Types
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.Tests.UnitTests (yellowColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)

raffleFormToConfig :: RaffleConfigFormState -> Value -> Maybe RaffleConfig
raffleFormToConfig RaffleConfigFormState {..} stake = do
  cddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _commitDdl)
  rddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _revealDdl)
  return $
    RaffleConfig
      { rCommitDDL = cddl
      , rRevealDDL = rddl
      , rTicketPrice = toLovelace $ fromIntegral _ticketPrice
      , rMinTickets = fromIntegral _minNoTickets
      , rStake = stake
      }

----------------------------------------------------------------------

-- *   Main State

------------------------------------------------------------------------------------------------

data RaffleizeUI = RaffleizeUI
  { providersCtx :: ProviderCtx
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , maybeSecretKey :: Maybe GYPaymentSigningKey
  , balance :: Value
  , logo :: String
  , message :: Text
  , mintTokenForm :: Form MintTokenFormState RaffleizeEvent NameResources
  , raffleConfigForm :: Form RaffleConfigFormState RaffleizeEvent NameResources
  , activeRafflesForm :: Form ActiveRafflesFormState RaffleizeEvent NameResources
  , myRafflesForm :: Form MyRafflesFormState RaffleizeEvent NameResources
  , buyTicketForm :: Form BuyTicketFormState RaffleizeEvent NameResources
  , myTicketsForm :: Form MyTicketsFormState RaffleizeEvent NameResources
  , myConstrctValueState :: ConstructValueState
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
    , appStartEvent = return
    , appAttrMap = const theMap
    }

readRaffleizeLogo :: IO String
readRaffleizeLogo = do
  putStrLn $ yellowColorString $ "Parsing Raffleize Logo at " <> show raffleizeLogoPath <> "..."
  readFile raffleizeLogoPath

tui :: IO ()
tui = do
  atlasConfig <- Data.Maybe.fromMaybe (error "Mandatory configuration file not found") <$> decodeConfigFile @GYCoreConfig atlasCoreConfig
  logo <- readRaffleizeLogo
  putStrLn $ yellowColorString "Loading Providers and starting the TUI..."
  initialState <- withCfgProviders atlasConfig "terminal user interface" $ \providers -> do
    buildInitialState (ProviderCtx atlasConfig providers) logo
  _endState <- defaultMain app initialState
  return ()

------------------------------------------------------------------------------------------------

-- *  Building the initial state

------------------------------------------------------------------------------------------------

buildInitialState :: ProviderCtx -> String -> IO RaffleizeUI
buildInitialState pCtx logo = do
  liftIO $ print ("BUILDING INITIAL STATE" :: String)
  maybeValidatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  maybeSKey <- readPaymentKeyFile operationSkeyFilePath
  allRafflesInfo <- getActiveRaffles pCtx
  let mintTokenForm = mkMintTokenForm (MintTokenFormState "test-tokens" 1)
  let buyTicketForm = mkBuyTicketForm (BuyTicketFormState mempty mempty)
  let activeRafflesForm = mkActiveRafflesForm (ActiveRafflesFormState (Data.Vector.fromList allRafflesInfo) Nothing)
  (myRafflesForm, myTicketsForm, raffleConfigForm, balance) <- case maybeSKey of
    Nothing -> do
      let rf = mkMyRafflesForm (MyRafflesFormState mempty Nothing)
      let tf = mkMyTicketsForm (MyTicketsFormState mempty Nothing)
      let cf = mkRaffleConfigForm (RaffleConfigFormState mempty mempty 0 0 mempty)
      return (rf, tf, cf, mempty)
    Just sKey -> do
      -- let myRafflesIds = getMyRaffleIdsFromValue val
      -- let myRafflesInfo = filter (\ri -> rRaffleID (riRsd ri) `elem` myRafflesIds) allRafflesInfo
      (addr, balance) <- getAddressAndValue pCtx sKey
      myRafflesInfo <- getMyRaffles pCtx addr
      myTicketsInfo <- getMyTickets pCtx addr
      now <- getCurrentTime
      let icd = addUTCTime 864000 now -- + 10 days
      let ird = addUTCTime 864000 icd
      let commitSuggestion = Data.Text.pack . iso8601Show $ icd
      let revealSuggestion = Data.Text.pack . iso8601Show $ ird
      let cf = mkRaffleConfigForm (RaffleConfigFormState commitSuggestion revealSuggestion 5 1 mempty)
      let rf = mkMyRafflesForm (MyRafflesFormState (Data.Vector.fromList myRafflesInfo) Nothing)
      let tf = mkMyTicketsForm (MyTicketsFormState (Data.Vector.fromList myTicketsInfo) Nothing)
      return (rf, tf, cf, balance)

  let constructValueState = mkConstructValueState balance mempty
  return (RaffleizeUI pCtx maybeValidatorsConfig maybeSKey balance logo mempty mintTokenForm raffleConfigForm activeRafflesForm myRafflesForm buyTicketForm myTicketsForm constructValueState MainScreen)

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
          ConstructValueScreen -> do
            let currentCVS = myConstrctValueState s
            let atvForm = addToValueForm currentCVS
            let isValidValue = allFieldsValid atvForm
            let atvFormState = formState atvForm
            let currentFocus = focusGetCurrent $ elementFocus currentCVS
            case key of
              KEnter ->
                if isValidValue && not (null (constructedValueList currentCVS))
                  then continue s {currentScreen = CreateRaffleScreen}
                  else continue s
              KChar '\t' ->
                do
                  let nextFocus = focusNext (elementFocus currentCVS)
                  let updatedCVS = currentCVS {elementFocus = nextFocus}
                  continue s {myConstrctValueState = updatedCVS}
              KIns -> do
                let selectedElement = snd <$> listSelectedElement (availableValueList currentCVS)
                case (selectedElement, isValidValue, (`elem` [1, 2]) <$> currentFocus) of
                  (Just (cs, tn, total), True, Just True) -> do
                    let currentAmount = formState atvForm ^. amount
                    let el = (cs, tn, min total (fromIntegral currentAmount))
                    let elementValue = unFlattenValue [el]
                    let availableBalance = unFlattenValue $ toList $ listElements (availableValueList currentCVS)
                    let newAvailableBalance = availableBalance #- elementValue
                    let newAvailableList = listReplace (Data.List.reverse $ flattenValue newAvailableBalance) (Just 0) (availableValueList currentCVS)
                    let stakeBalance = unFlattenValue $ toList $ listElements (constructedValueList currentCVS)
                    let newStakeBalance = stakeBalance #+ elementValue
                    let newConstructedList = listReplace (Data.List.reverse $ flattenValue newStakeBalance) (Just 0) (constructedValueList currentCVS)
                    let atvForm1 = updateFormState atvFormState {_amount = 1} atvForm
                    let newcvs = currentCVS {constructedValueList = newConstructedList, availableValueList = newAvailableList, addToValueForm = atvForm1}
                    continue s {myConstrctValueState = newcvs}
                  _ -> continue s
              _ -> do
                case currentFocus of
                  Just 1 -> do
                    newAvailableList <- handleListEvent vtye (availableValueList currentCVS)
                    let newSelectedElement = snd <$> listSelectedElement newAvailableList
                    let mselectedElementAmount = (\(_, _, i) -> fromIntegral i) <$> newSelectedElement
                    case mselectedElementAmount of
                      Nothing -> continue s
                      Just selectedElementAmount -> do
                        let atvForm1 =
                              if key `elem` [KUp, KDown]
                                then do
                                  let currentAmount = atvFormState ^. amount
                                  let atvFormState1 = atvFormState {_amount = selectedElementAmount}
                                  if selectedElementAmount /= currentAmount then updateFormState atvFormState1 atvForm else atvForm
                                else atvForm
                        let fieldValidations =
                              [ setFieldValid (null newAvailableList || liftA2 (&&) (> 0) (<= selectedElementAmount) (formState atvForm1 ^. amount)) AddToValueAmountField
                              ]
                        let validated_form = foldr' ($) atvForm1 fieldValidations
                        let newcvs = currentCVS {availableValueList = newAvailableList, addToValueForm = validated_form}
                        continue s {myConstrctValueState = newcvs}
                  Just 2 -> do
                    atvForm1 <- handleFormEvent e (addToValueForm currentCVS)
                    let atvFormState1 = formState atvForm1
                    let avl = availableValueList currentCVS
                    let newSelectedElement = snd <$> listSelectedElement avl
                    let selectedElementAmount = maybe (0 :: Int) (\(_, _, i) -> fromIntegral i) newSelectedElement
                    let fieldValidations =
                          [ setFieldValid (null avl || liftA2 (&&) (> 0) (<= selectedElementAmount) (formState atvForm1 ^. amount)) AddToValueAmountField
                          ]
                    let validated_form = foldr' ($) atvForm1 fieldValidations
                    let newcvs = currentCVS {addToValueForm = validated_form}
                    continue s {myConstrctValueState = newcvs}
                  Just 3 -> case key of
                    KDel -> do
                      let selectedElement = listSelectedElement (constructedValueList currentCVS)
                      case selectedElement of
                        Nothing -> continue s
                        Just (idx, el) -> do
                          let newConstructedList = listRemove idx (constructedValueList currentCVS)
                          let elementValue = unFlattenValue [el]
                          let availableBalance = unFlattenValue $ toList $ listElements (availableValueList currentCVS)
                          let newAvailableBalance = availableBalance #+ elementValue
                          let newAvailableList = listReplace (Data.List.reverse $ flattenValue newAvailableBalance) (Just 0) (availableValueList currentCVS)
                          let newcvs = currentCVS {constructedValueList = newConstructedList, availableValueList = newAvailableList}
                          continue s {myConstrctValueState = newcvs}
                    _ -> do
                      newConstructedList <- handleListEvent vtye (constructedValueList currentCVS)
                      let newcvs = currentCVS {constructedValueList = newConstructedList}
                      continue s {myConstrctValueState = newcvs}
                  _ -> error "invalid focus"
          MyTicketsScreen -> do
            let mtForm = myTicketsForm s
                mtFormState = formState mtForm
                mst = mtFormState ^. selectedTicket
             in case key of
                  KChar 's' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case mst of
                          Just ticketInfo -> do
                            let contextNFT = fst $ generateTicketACFromTicket (tiTsd ticketInfo)
                            liftIO clearScreen
                            txOutRef <- liftIO $ revealTicket (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ maybeSecretKey s) (Data.Text.unpack "marius") contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                            continue
                              initialState
                                { message = "TICKET SECRET REVEALED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  _ -> do
                    mtForm1 <- handleFormEvent e (myTicketsForm s)
                    continue s {myTicketsForm = mtForm1}
          BuyTicketScreen ->
            let btForm = buyTicketForm s
                btFormState = formState btForm
                secretString = Data.Text.unpack $ btFormState ^. secret
                mRecipient = addressFromTextMaybe $ btFormState ^. ticketRecipient
                arState = formState (activeRafflesForm s)
                contextNFT = rRaffleID $ riRsd $ fromJust $ arState ^. selectedRaffle
                invalid_fields = invalidFields btForm
             in case key of
                  KEnter -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        if null invalid_fields
                          then do
                            liftIO clearScreen
                            txOutRef <- liftIO $ buyTicket (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust (maybeSecretKey s)) secretString contextNFT mRecipient
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                            continue
                              initialState
                                { message = "TICKET BOUGHT SUCCESFULLY!\n" <> showLink nid "tx" txOutRef <> "\n FOR RAFFLE " <> showText contextNFT
                                , currentScreen = MainScreen
                                }
                          else continue s
                  _ -> do
                    updated_form <- handleFormEvent e btForm
                    continue s {buyTicketForm = updated_form}
          MyRafflesScreen ->
            let mrForm = myRafflesForm s
                mrFormState = formState mrForm
                msr = mrFormState ^. selectedMyRaffle
             in case key of
                  KChar 'c' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case msr of
                          Just sr -> do
                            let contextNFT = rRaffleID $ riRsd sr
                            liftIO clearScreen
                            txOutRef <- liftIO $ cancelRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ maybeSecretKey s) contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                            continue
                              initialState
                                { message = "RAFFLE CANCELLED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  KChar 'r' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case msr of
                          Just sr -> do
                            let contextNFT = rRaffleID $ riRsd sr
                            liftIO clearScreen
                            txOutRef <- liftIO $ recoverStakeRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ maybeSecretKey s) contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                            continue
                              initialState
                                { message = "RAFFLE STAKE RECOVERED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  KChar 'e' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case msr of
                          Just sr -> do
                            let contextNFT = rRaffleID $ riRsd sr
                            liftIO clearScreen
                            txOutRef <- liftIO $ recoverStakeAndAmountRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ maybeSecretKey s) contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                            continue
                              initialState
                                { message = "RAFFLE STAKE AND COLLECTED AMOUNT RECOVERED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  _ -> do
                    mrForm1 <- handleFormEvent e (myRafflesForm s)
                    continue s {myRafflesForm = mrForm1}
          ActiveRafflesScreen ->
            case key of
              KChar 'b' -> do
                case formState (activeRafflesForm s) ^. selectedRaffle of
                  Just sr -> do
                    let isValidBuy = any ((== "BuyTicket") . snd) $ riAvailableActions sr
                    if isValidBuy then continue s {currentScreen = BuyTicketScreen} else continue s
                  _ -> continue s
              _ -> do
                arForm <- handleFormEvent e (activeRafflesForm s)
                continue s {activeRafflesForm = arForm}
          CreateRaffleScreen ->
            let crForm = raffleConfigForm s
                invalid_fields = invalidFields crForm
             in case key of
                  KEnter -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        let currentCVS = myConstrctValueState s
                        let stake = unFlattenValue $ toList $ listElements (constructedValueList currentCVS)
                        let mraffle = raffleFormToConfig (formState crForm) stake
                        if null invalid_fields && isJust mraffle
                          then do
                            liftIO clearScreen
                            let raffle = fromJust mraffle
                            let recpient = addressFromTextMaybe $ _raffleRecipient (formState crForm)
                            txOutRef <- liftIO $ createRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust (maybeSecretKey s)) raffle recpient
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                            continue initialState {message = "RAFFLE SUCCESFULLY CREATED !\n" <> showLink nid "tx" txOutRef}
                          else continue s
                  KIns -> continue s {currentScreen = ConstructValueScreen}
                  _ -> do
                    crForm1 <- handleFormEvent e crForm
                    let crFormState = formState crForm1
                    let fieldValidations =
                          [ setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState ^. commitDdl))) CommitDdlField
                          , setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState ^. revealDdl))) RevealDdlField
                          , setFieldValid (crFormState ^. ticketPrice > (fromIntegral (rMinTicketPrice mockRaffleParam) `div` 1000000)) TicketPriceField
                          , setFieldValid (liftA2 (&&) (> 0) (< fromIntegral (rMaxNoOfTickets mockRaffleParam)) $ crFormState ^. minNoTickets) MinNoTokensField
                          , setFieldValid ((not . null) (toList (constructedValueList (myConstrctValueState s)))) ConstructedValueItemsList
                          ]
                    let validated_form = foldr' ($) crForm1 fieldValidations
                    continue s {raffleConfigForm = validated_form}
          MintTokenScreen -> handleMintTokensEvents s e
          MainScreen -> case key of
            (KChar c) -> case c of
              'R' -> continue s {message = "Refresh Screen"}
              'q' -> halt s
              'g' -> do
                liftIO $ generateNewSkey operationSkeyFilePath
                s' <- liftIO $ buildInitialState (providersCtx s) (logo s)
                continue s'
              'e' -> do
                liftIO $ sequence_ [exportRaffleScript, exportTicketScript, exportMintingPolicy]
                continue s {message = "VALIDATORS SUCCESFULLY EXPORTED !\n" <> Data.Text.pack (Data.List.intercalate "\n" [raffleizeValidatorFile, ticketValidatorFile, mintingPolicyFile])}
              'v' -> continue s {currentScreen = ActiveRafflesScreen}
              _ ->
                if isJust (maybeSecretKey s)
                  then do
                    case c of
                      'm' -> continue s {currentScreen = MintTokenScreen}
                      'c' -> do
                        continue
                          s
                            { currentScreen = CreateRaffleScreen
                            }
                      'l' -> do
                        liftIO clearScreen
                        initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                        continue initialState {message = "Updated"}
                      'd' -> do
                        liftIO clearScreen
                        liftIO $ deployValidators (providersCtx s) (fromJust (maybeSecretKey s))
                        initialState <- liftIO $ buildInitialState (providersCtx s) (logo s)
                        continue initialState {message = "VALIDATORS SUCCESFULLY DEPLOYED !\nTxOuts references are saved to " <> showText raffleizeValidatorsConfig}
                      'r' -> do
                        continue s {currentScreen = MyRafflesScreen}
                      't' -> do
                        continue s {currentScreen = MyTicketsScreen}
                      _ -> continue s
                  else continue s
            _ -> continue s
      _ -> continue s
    _ -> continue s

-- | Handle events in Mint Tokens Screen
handleMintTokensEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources (Next RaffleizeUI)
handleMintTokensEvents s@RaffleizeUI {currentScreen, maybeSecretKey, mintTokenForm, providersCtx} event
  | currentScreen == MintTokenScreen =
      let mtForm = mintTokenForm
       in case (event, maybeSecretKey) of
            (VtyEvent (EvKey key _modifiers), Just secretKey) ->
              case (key, allFieldsValid mtForm) of
                (KEnter, True) ->
                  do
                    liftIO clearScreen
                    let mtFormState = formState mtForm
                    txOutRef <-
                      liftIO $
                        mintTestTokens
                          providersCtx
                          secretKey
                          (Data.Text.unpack (mtFormState ^. tokenNameField))
                          (fromIntegral (mtFormState ^. mintAmount))
                    let nid = (cfgNetworkId . ctxCoreCfg) providersCtx
                    initialState <- liftIO $ buildInitialState providersCtx (logo s)
                    continue
                      initialState
                        { message = "TEST TOKENS SUCCESFULLY MINTED !\n" <> showLink nid "tx" txOutRef
                        }
                _ -> do
                  newMtForm <- handleFormEvent event mtForm
                  let newMtFormState = formState newMtForm
                  let fieldValidations =
                        [ setFieldValid (Data.Text.length (newMtFormState ^. tokenNameField) <= tokenNameMaxLength) TokenNameField
                        , setFieldValid (newMtFormState ^. mintAmount > 0) MintAmountField
                        ]
                  let validated_form = foldr' ($) newMtForm fieldValidations
                  continue s {mintTokenForm = validated_form}
            _ -> continue s
handleMintTokensEvents _state _event = error "Invalid use of handleMintTokensEvents"

------------------------------------------------------------------------------------------------

-- *  Drawing

------------------------------------------------------------------------------------------------

drawUI :: RaffleizeUI -> [Widget NameResources]
drawUI s =
  let mSelectedRaffle = rRaffleID . riRsd <$> formState (activeRafflesForm s) ^. selectedRaffle
   in joinBorders . withBorderStyle unicode . borderWithLabel (txt " RAFFLEIZE - C.A.R.D.A.N.A ")
        <$> [ if Data.Text.null (message s) then emptyWidget else center (withAttr "highlight" $ txt (message s)) <=> txt "[ESC] - Close"
            , if currentScreen s == MintTokenScreen then drawMintTestTokensForm (mintTokenForm s) else emptyWidget
            , if currentScreen s == CreateRaffleScreen then createRaffleScreen (raffleConfigForm s) (myConstrctValueState s) else emptyWidget
            , if currentScreen s == BuyTicketScreen then drawBuyTicketForm mSelectedRaffle (buyTicketForm s) else emptyWidget
            , if currentScreen s == ActiveRafflesScreen then drawActiveRafflesForm (activeRafflesForm s) else emptyWidget
            , if currentScreen s == MyRafflesScreen then drawMyRafflesForm (myRafflesForm s) else emptyWidget
            , if currentScreen s == MyTicketsScreen then drawMyTicketsScreen (myTicketsForm s) else emptyWidget
            , if currentScreen s == ConstructValueScreen then drawConstructValueWidget (myConstrctValueState s) else emptyWidget
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
    , center bodyWidget
    ]
  where
    bodyWidget :: Widget NameResources
    bodyWidget =
      let balanceList = Brick.Widgets.List.list ValueItemsList (flattenValue (balance s)) 5
       in vBox $
            padLeftRight 1
              <$> [ summaryWidget
                  , hBorder
                  , assetsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (maybeSecretKey s) balanceList
                  ]
      where
        summaryWidget :: Widget NameResources
        summaryWidget =
          hCenter $
            hBox
              [ availableActionsWidget
              , providersWidget ((ctxCoreCfg . providersCtx) s)
              , validatorsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (validatorsConfig s)
              , walletFlagWidget (maybeSecretKey s)
              ]
          where
            validatorsWidget :: GYNetworkId -> Maybe RaffleizeTxBuildingContext -> Widget n
            validatorsWidget nid mv =
              borderWithLabel (txt "VALIDATORS") $
                renderTable $
                  table $
                    [txt "Deployed: ", symbolWidget (isJust mv)] : case mv of
                      (Just (RaffleizeTxBuildingContext {..})) ->
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
            providersWidget :: GYCoreConfig -> Widget n
            providersWidget cfg =
              borderWithLabel (txt "BLOCKCHAIN PROVIDER") $
                renderTable $
                  table
                    [ [txt "Loaded: ", symbolWidget True]
                    , [txt "Provider: ", printProvider cfg]
                    , [txt "Network: ", printNetwork cfg]
                    ]
            availableActionsWidget :: Widget n
            availableActionsWidget =
              withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
                vBox
                  ( txt
                      <$> filter
                        (not . Data.Text.null)
                        ( ( if isNothing (maybeSecretKey s)
                              then ["[G] - Generate new wallet skey"]
                              else
                                [ "[V] - View all active raffles"
                                , "[R] - View my raffles"
                                , "[T] - View my tickets"
                                , "[C] - Create raffle" -- TODO : CHECK ADA BALANCE
                                , "[M] - Mint some test tokens"
                                , "[D] - Deploy Raffleize Validators"
                                , "[L] - Get wallet balance"
                                ]
                          )
                            ++ [ "[E] - Export validators"
                               , "[Q] - Quit"
                               ]
                        )
                  )
    assetsWidget :: GYNetworkId -> Maybe GYPaymentSigningKey -> GenericList NameResources [] (CurrencySymbol, TokenName, Integer) -> Widget NameResources
    assetsWidget nid (Just skey) valList =
      let addr = addressFromPaymentSigningKey nid skey
          val = unFlattenValue $ listElements valList
       in borderWithLabel (txt "WALLET") $
            hBox
              [ assetsAdaWidget addr val
              , vBorder
              , assetsBalanceWidget valList
              ]
      where
        assetsAdaWidget :: GYAddress -> Value -> Widget NameResources
        assetsAdaWidget addr val =
          renderTable $
            table
              [ [txt "Address: ", addressWidget]
              , [txt "Ada | ₳ |:", adaBalanceWidget]
              ]
          where
            addressWidget :: Widget n
            addressWidget =
              let addrText = addressToText addr
               in hyperlink (showLink nid "address" addrText) $ withAttr "good" $ txt addrText
            adaBalanceWidget :: Widget n
            adaBalanceWidget = withAttr "good" $ txt (showText (fromValue val) <> "\n")
    assetsWidget _ _ _ = emptyWidget
    assetsBalanceWidget :: (Traversable t, Splittable t) => GenericList NameResources t (CurrencySymbol, TokenName, Integer) -> Widget NameResources
    assetsBalanceWidget valueItemsList =
      vLimit 100 $
        hLimit 150 $
          borderWithLabel (txt "ASSETS") $
            -- visible $
            --   withVScrollBarHandles $
            --     withVScrollBars OnRight $
            --       viewport ValueItemsViewPort Vertical $
            --         vLimit 300 $
            --           hLimit 150 $
            renderList (valueItemWidget True) False valueItemsList

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

---------

module UI where

import Actions
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
import Control.Lens
import Control.Monad.IO.Class
import Data.List qualified
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text qualified
import Data.Text.IO qualified
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified
import GHC.Real (Integral (div))
import GeniusYield.GYConfig
import GeniusYield.Types
import Graphics.Vty
import PlutusLedgerApi.V1.Value
  ( AssetClass,
    CurrencySymbol,
    TokenName,
    Value,
    flattenValue,
  )
import PlutusPrelude (showText)
import PlutusTx.Builtins qualified
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.OnChain.RaffleizeLogic
  ( actionToLabel,
    generateTicketACFromTicket,
  )
import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import RaffleizeDApp.Utils
import RaffleizeWidgets
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)
import Types

----------------------------------------------------------------------

-- *   Main State

------------------------------------------------------------------------------------------------

data RaffleizeUI = RaffleizeUI
  { providersCtx :: ProviderCtx,
    validatorsConfig :: Maybe RaffleizeTxBuildingContext,
    maybeSecretKey :: Maybe GYExtendedPaymentSigningKey,
    balance :: Value,
    balanceList :: GenericList NameResources [] (CurrencySymbol, TokenName, Integer),
    logo :: String,
    message :: Text,
    mnemonicForm :: Form MnemonicFormState RaffleizeEvent NameResources,
    mintTokenForm :: Form MintTokenFormState RaffleizeEvent NameResources,
    raffleConfigForm :: Form RaffleConfigFormState RaffleizeEvent NameResources,
    activeRafflesForm :: Form ActiveRafflesFormState RaffleizeEvent NameResources,
    updatingRaffle :: Maybe RaffleInfo,
    myRafflesForm :: Form MyRafflesFormState RaffleizeEvent NameResources,
    buyTicketForm :: Form BuyTicketFormState RaffleizeEvent NameResources,
    myTicketsForm :: Form MyTicketsFormState RaffleizeEvent NameResources,
    revealSecretForm :: Form RevealSecretFormState RaffleizeEvent NameResources,
    myConstrctValueState :: ConstructValueState,
    currentScreen :: Screen
  }

------------------------------------------------------------------------------------------------

-- *   App definition

------------------------------------------------------------------------------------------------

app :: App RaffleizeUI RaffleizeEvent NameResources
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
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
  maybeValidatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  maybeSKey <- readMnemonicFile operationPrivFilePath
  allRafflesInfo <- getActiveRaffles pCtx
  let mnemonicForm = mkMnemonicForm (MnemonicFormState mempty)
  let mintTokenForm = mkMintTokenForm (MintTokenFormState "test-tokens" 1)
  let buyTicketForm = mkBuyTicketForm (BuyTicketFormState mempty mempty)
  let activeRafflesForm = mkActiveRafflesForm (ActiveRafflesFormState (Data.Vector.fromList allRafflesInfo) Nothing)
  let revealSecretForm = mkRevealSecretForm (RevealSecretFormState mempty)
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
  let balanceList = Brick.Widgets.List.list ValueItemsList (flattenValue balance) 5
  let constructValueState = mkConstructValueState balance mempty
  return (RaffleizeUI pCtx maybeValidatorsConfig maybeSKey balance balanceList logo mempty mnemonicForm mintTokenForm raffleConfigForm activeRafflesForm Nothing myRafflesForm buyTicketForm myTicketsForm revealSecretForm constructValueState MainScreen)

refreshState :: RaffleizeUI -> IO RaffleizeUI
refreshState RaffleizeUI {..} = do
  putStrLn $ yellowColorString "Refreshing info ..."
  buildInitialState providersCtx logo

------------------------------------------------------------------------------------------------

-- *  Handling Events

------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleEvent e = do
  s <- get
  case e of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> put $ s {message = "", currentScreen = MainScreen}
      EvKey key _modifiers ->
        case currentScreen s of
          ImportWalletFromMnemonic -> handleMnemonicEvents s e
          MintTokenScreen -> handleMintTokensEvents s e
          ActiveRafflesScreen -> handleActiveRafflesEvents s e
          CreateRaffleScreen -> handleCreateUpdateRaffleEvents s e
          UpdateRaffleScreen -> handleCreateUpdateRaffleEvents s e
          ConstructValueScreen -> handleConstrutValueEvents s e
          BuyTicketScreen -> handleBuyTicketScreenEvents s e
          MyTicketsScreen -> handleMyTicketsEvents s e
          RevealTicketSecretScreen -> handleRevealSecretEvents s e
          MyRafflesScreen -> handleMyRafflesEvents s e
          MainScreen ->
            if not . Data.Text.null $ message s
              then put s
              else case key of
                (KChar c) -> case c of
                  'q' -> halt
                  'v' -> put $ s {currentScreen = ActiveRafflesScreen}
                  'i' -> put $ s {currentScreen = ImportWalletFromMnemonic}
                  'e' -> do
                    liftIO $ sequence_ [exportRaffleScript, exportTicketScript, exportMintingPolicy]
                    put $ s {message = "VALIDATORS SUCCESSFULLY EXPORTED !\n" <> Data.Text.pack (Data.List.intercalate "\n" [raffleizeValidatorFile, ticketValidatorFile, mintingPolicyFile])}
                  _ ->
                    if isJust (maybeSecretKey s)
                      then do
                        case c of
                          'm' -> put $ s {currentScreen = MintTokenScreen}
                          'c' -> put $ s {currentScreen = CreateRaffleScreen}
                          'r' -> put $ s {currentScreen = MyRafflesScreen}
                          't' -> put $ s {currentScreen = MyTicketsScreen}
                          'l' -> do
                            liftIO clearScreen
                            initialState <- liftIO $ refreshState s
                            put $ initialState {message = "Updated"}
                          'd' -> do
                            liftIO clearScreen
                            liftIO $ deployValidators (providersCtx s) (fromJust (maybeSecretKey s))
                            initialState <- liftIO $ refreshState s
                            put $ initialState {message = "VALIDATORS SUCCESSFULLY DEPLOYED !\nTxOuts references are saved to " <> showText raffleizeValidatorsConfig}
                          _ -> put s
                      else put s
                _ -> do
                  (newBalanceList, ()) <- nestEventM (balanceList s) $ handleListEvent vtye
                  put s {balanceList = newBalanceList}
      _ -> put s
    _ -> put s

handleConstrutValueEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleConstrutValueEvents s@RaffleizeUI {..} e | currentScreen == ConstructValueScreen =
  case e of
    VtyEvent vtye@(EvKey key _modifiers) ->
      do
        let atvForm = addToValueForm myConstrctValueState
        let isValidAmountField = allFieldsValid atvForm
        let atvFormState = formState atvForm
        let currentFocus = elementFocus myConstrctValueState
        let currentFocusValue = focusGetCurrent currentFocus
        let myAvailableValueList = availableValueList myConstrctValueState
        let myConstructedValueList = constructedValueList myConstrctValueState
        case key of
          KEnter -> if isValidAmountField then if isNothing updatingRaffle then put $ s {currentScreen = CreateRaffleScreen} else put $ s {currentScreen = UpdateRaffleScreen} else put s
          KChar '\t' -> put $ s {myConstrctValueState = myConstrctValueState {elementFocus = focusNext currentFocus}}
          k | k == KChar '+' || k == KIns -> do
            let selectedElement = snd <$> listSelectedElement myAvailableValueList
            case (selectedElement, isValidAmountField, (`elem` [1, 2]) <$> currentFocusValue) of
              (Just (cs, tn, total), True, Just True) -> do
                let currentAmount = atvFormState ^. amount
                let elementOfConstructedValue = unFlattenValue [(cs, tn, min total (fromIntegral currentAmount))]
                let availableBalance = unFlattenValue $ toList $ listElements myAvailableValueList
                let newAvailableBalance = availableBalance #- elementOfConstructedValue
                let newAvailableList = listReplace (Data.List.reverse $ flattenValue newAvailableBalance) (Just 0) myAvailableValueList
                let stakeBalance = unFlattenValue $ toList $ listElements myConstructedValueList
                let newStakeBalance = stakeBalance #+ elementOfConstructedValue
                let newConstructedList = listReplace (Data.List.reverse $ flattenValue newStakeBalance) (Just 0) myConstructedValueList
                let newAtvFormState = updateFormState atvFormState {_amount = 1} atvForm
                put $ s {myConstrctValueState = myConstrctValueState {constructedValueList = newConstructedList, availableValueList = newAvailableList, addToValueForm = newAtvFormState}}
              _ -> put s
          _ -> do
            case currentFocusValue of
              Just 1 -> do
                (newAvailableList, ()) <- nestEventM myAvailableValueList $ handleListEvent vtye
                let newSelectedElement = snd <$> listSelectedElement newAvailableList
                let mselectedElementAmount = (\(_, _, i) -> fromIntegral i) <$> newSelectedElement
                case mselectedElementAmount of
                  Nothing -> put s
                  Just selectedElementAmount -> do
                    let newAtvFormState =
                          if key `elem` [KUp, KDown]
                            then do
                              let currentAmount = atvFormState ^. amount
                              if selectedElementAmount /= currentAmount then updateFormState (atvFormState {_amount = selectedElementAmount}) atvForm else atvForm
                            else atvForm
                    let fieldValidations =
                          [ setFieldValid (null newAvailableList || liftA2 (&&) (> 0) (<= selectedElementAmount) (formState newAtvFormState ^. amount)) AddToValueAmountField
                          ]
                    let validatedForm = foldr' ($) newAtvFormState fieldValidations
                    put $ s {myConstrctValueState = myConstrctValueState {availableValueList = newAvailableList, addToValueForm = validatedForm}}
              Just 2 -> do
                (newAtvForm, ()) <- nestEventM (addToValueForm myConstrctValueState) $ handleFormEvent e
                let newAtvFormState = formState newAtvForm
                let avl = availableValueList myConstrctValueState
                let newSelectedElement = snd <$> listSelectedElement avl
                let selectedElementAmount = maybe (0 :: Int) (\(_, _, i) -> fromIntegral i) newSelectedElement
                let fieldValidations =
                      [ setFieldValid (null avl || liftA2 (&&) (> 0) (<= selectedElementAmount) (newAtvFormState ^. amount)) AddToValueAmountField
                      ]
                let validatedForm = foldr' ($) newAtvForm fieldValidations
                put $ s {myConstrctValueState = myConstrctValueState {addToValueForm = validatedForm}}
              Just 3 -> case key of
                k | k == KChar '-' || k == KDel -> do
                  let selectedElement = listSelectedElement myConstructedValueList
                  case selectedElement of
                    Nothing -> put s
                    Just (idx, el) -> do
                      let newConstructedList = listRemove idx myConstructedValueList
                      let elementValue = unFlattenValue [el]
                      let availableBalance = unFlattenValue $ toList $ listElements myAvailableValueList
                      let newAvailableBalance = availableBalance #+ elementValue
                      let newAvailableList = listReplace (Data.List.reverse $ flattenValue newAvailableBalance) (Just 0) myAvailableValueList
                      let newcvs = myConstrctValueState {constructedValueList = newConstructedList, availableValueList = newAvailableList}
                      put $ s {myConstrctValueState = newcvs}
                _ -> do
                  (newConstructedList, ()) <- nestEventM myConstructedValueList $ handleListEvent vtye
                  put $ s {myConstrctValueState = myConstrctValueState {constructedValueList = newConstructedList}}
              _ -> error "invalid focus"
    _ -> put s
handleConstrutValueEvents _state _event = error "Invalid use of handleConstrutValueEvents"

-- Refactor helper functions
raffleizeTransactionHandler :: RaffleizeOffchainContext -> GYExtendedPaymentSigningKey -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> RaffleizeUI -> Bool -> EventM NameResources RaffleizeUI ()
raffleizeTransactionHandler roc@(RaffleizeOffchainContext _ providersCtx) secretKey raffleizeAction contextNFT mAddr s validateAction =
  let nid = cfgNetworkId . ctxCoreCfg $ providersCtx
   in if validateAction
        then do
          liftIO clearScreen
          liftIO $ raffleizeActionToIntro contextNFT raffleizeAction
          txOutRef <- liftIO $ raffleizeTransaction roc secretKey raffleizeAction contextNFT mAddr
          initialState <- liftIO $ refreshState s
          liftIO clearScreen
          let successMessage = "Transaction confirmed ! \n You transaction is now onchain: \n\t"
          put $ initialState {message = successMessage <> showLink nid "tx" txOutRef <> "\n"}
        else put s

raffleFormToConfig :: RaffleConfigFormState -> Value -> Maybe RaffleConfig
raffleFormToConfig RaffleConfigFormState {..} stake = do
  cddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _commitDdl)
  rddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _revealDdl)
  return $
    RaffleConfig
      { rCommitDDL = cddl,
        rRevealDDL = rddl,
        rTicketPrice = toLovelace $ fromIntegral _ticketPrice,
        rMinTickets = fromIntegral _minNoTickets,
        rStake = stake
      }

-- | Handle events Create Raffle Screen
handleCreateUpdateRaffleEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleCreateUpdateRaffleEvents s@RaffleizeUI {..} event | currentScreen == CreateRaffleScreen || currentScreen == UpdateRaffleScreen = do
  let crForm = raffleConfigForm
      myConstructedValueList = constructedValueList myConstrctValueState
      invalid_fields = invalidFields crForm <> ([ConstructedValueItemsList | (not . (`elem` [1, 2]) . length) (toList myConstructedValueList)])
   in case (event, maybeSecretKey, validatorsConfig) of
        (VtyEvent (EvKey key _modifiers), Just secretKey, Just validatorsTxOutRefs) ->
          case (key, null invalid_fields) of
            (KEnter, True) ->
              do
                let stake = unFlattenValue $ toList $ listElements myConstructedValueList
                let mRaffleConfig = raffleFormToConfig (formState crForm) stake
                let recpient = addressFromTextMaybe $ _raffleRecipient (formState crForm)
                case mRaffleConfig of
                  Just rConfig -> do
                    let (raffleizeAction, contextNFT) = if currentScreen == CreateRaffleScreen then (User (CreateRaffle rConfig), Nothing) else (RaffleOwner (Update rConfig), rRaffleID . riRsd <$> updatingRaffle)
                    raffleizeTransactionHandler (RaffleizeOffchainContext validatorsTxOutRefs providersCtx) secretKey raffleizeAction contextNFT recpient s True
                  Nothing -> put s
            (k, _) | k == KChar '+' || k == KIns -> put $ s {currentScreen = ConstructValueScreen}
            _ -> do
              (newFormState, ()) <- nestEventM crForm $ handleFormEvent event
              let crFormState = formState newFormState
              let fieldValidations =
                    [ setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState ^. commitDdl))) CommitDdlField,
                      setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState ^. revealDdl))) RevealDdlField,
                      setFieldValid (crFormState ^. ticketPrice > (fromIntegral (rMinTicketPrice mockRaffleParam) `div` 1000000)) TicketPriceField,
                      setFieldValid (liftA2 (&&) (> 0) (< fromIntegral (rMaxNoOfTickets mockRaffleParam)) $ crFormState ^. minNoTickets) MinNoTokensField
                    ]
              let validatedForm = foldr' ($) newFormState fieldValidations
              put $ s {raffleConfigForm = validatedForm}
        _ -> do
          (newFormState, ()) <- nestEventM crForm $ handleFormEvent event
          put $ s {raffleConfigForm = newFormState}
handleCreateUpdateRaffleEvents _state _event = error "Invalid use of handleCreateUpdateRaffleEvents"

-- | Handle events in View Active Raffles Screen
handleActiveRafflesEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleActiveRafflesEvents s@RaffleizeUI {..} event | currentScreen == ActiveRafflesScreen = do
  let maybeSelectedRaffle = formState activeRafflesForm ^. selectedRaffle
  case (event, maybeSelectedRaffle) of
    (VtyEvent (EvKey (KChar 'b') _modifiers), Just selectedActiveRaffle) -> do
      let isValidBuy = any ((== "BuyTicket") . snd) $ riAvailableActions selectedActiveRaffle
      if isValidBuy then put s {currentScreen = BuyTicketScreen} else put s
    _ -> do
      (arForm, ()) <- nestEventM activeRafflesForm $ handleFormEvent event
      put s {activeRafflesForm = arForm}
handleActiveRafflesEvents _state _event = error "Invalid use of handleActiveRafflesEvents"

-- | Handle events in Buy Ticket Secret Screen
handleBuyTicketScreenEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleBuyTicketScreenEvents s@RaffleizeUI {..} event | currentScreen == BuyTicketScreen = do
  let btForm = buyTicketForm
      btFormState = formState btForm
      secretString = Data.Text.unpack $ btFormState ^. secret
      mRecipient = addressFromTextMaybe $ btFormState ^. ticketRecipient
      arState = formState activeRafflesForm
      maybeSelectedRaffle = arState ^. selectedRaffle
      invalid_fields = invalidFields btForm
   in case (event, maybeSecretKey, validatorsConfig, maybeSelectedRaffle, null invalid_fields) of
        (VtyEvent (EvKey KEnter _modifiers), Just secretKey, Just validatorsTxOutRefs, Just selectedActiveRaffle, True) ->
          do
            let contextNFT = rRaffleID . riRsd $ selectedActiveRaffle
            let secretHash = PlutusTx.Builtins.blake2b_256 $ fromString @BuiltinByteString secretString
            let isAllowedToBuy = ("User", "BuyTicket") `elem` riAvailableActions selectedActiveRaffle
            raffleizeTransactionHandler (RaffleizeOffchainContext validatorsTxOutRefs providersCtx) secretKey (User (BuyTicket (SecretHash secretHash))) (Just contextNFT) mRecipient s isAllowedToBuy
        _ -> do
          (updated_form, ()) <- nestEventM btForm $ handleFormEvent event
          put $ s {buyTicketForm = updated_form}
handleBuyTicketScreenEvents _state _event = error "Invalid use of handleBuyTicketScreenEvents"

-- | Handle events in Reveal Ticket Secret Screen
handleRevealSecretEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleRevealSecretEvents s@RaffleizeUI {..} event
  | currentScreen == RevealTicketSecretScreen =
      let mtForm = myTicketsForm
          mtFormState = formState mtForm
          mySelectedTicket = mtFormState ^. selectedTicket
       in case (event, maybeSecretKey, validatorsConfig, mySelectedTicket) of
            (VtyEvent (EvKey key _modifiers), Just secretKey, Just validatorsTxOutRefs, Just selectedTicketInfo) -> do
              case key of
                KEnter -> do
                  let revealedTicketSecret = Data.Text.unpack $ formState revealSecretForm ^. revealedSecret
                  let revealedSecretBS = fromString @BuiltinByteString revealedTicketSecret
                  let contextNFT = fst $ generateTicketACFromTicket (tiTsd selectedTicketInfo)
                  let isAllowedToReveal = ("TicketOwner", "RevealTicketSecret") `elem` tiAvailableActions selectedTicketInfo
                  if PlutusTx.Builtins.blake2b_256 revealedSecretBS #== unSecretHash (tSecretHash (tiTsd selectedTicketInfo))
                    then raffleizeTransactionHandler (RaffleizeOffchainContext validatorsTxOutRefs providersCtx) secretKey (TicketOwner (RevealTicketSecret revealedSecretBS)) (Just contextNFT) Nothing s isAllowedToReveal
                    else put s
                _ -> do
                  (newRevealSecretForm, ()) <- nestEventM revealSecretForm $ handleFormEvent event
                  let newRevealedTicketSecret = Data.Text.unpack $ formState newRevealSecretForm ^. revealedSecret
                  let newRevealedSecretBS = fromString @BuiltinByteString newRevealedTicketSecret
                  let fieldValidations =
                        [ setFieldValid
                            (PlutusTx.Builtins.blake2b_256 newRevealedSecretBS #== unSecretHash (tSecretHash (tiTsd selectedTicketInfo)))
                            RevealedSecretField
                        ]
                  let validatedForm = foldr' ($) newRevealSecretForm fieldValidations
                  put $ s {revealSecretForm = validatedForm}
            _ -> put s
handleRevealSecretEvents _state _event = error "Invalid use of handleRevealSecretEvents"

-- | Handle events in My Tickets Screen
handleMyTicketsEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleMyTicketsEvents s@RaffleizeUI {..} event
  | currentScreen == MyTicketsScreen = do
      let mtForm = myTicketsForm
          mtFormState = formState mtForm
          mySelectedTicket = mtFormState ^. selectedTicket
      case (event, maybeSecretKey, validatorsConfig) of
        (VtyEvent (EvKey key _modifiers), Just secretKey, Just validatorsTxOutRefs) ->
          case mySelectedTicket of
            Just selectedTicketInfo -> do
              let contextNFT = fst $ generateTicketACFromTicket (tiTsd selectedTicketInfo)
                  actionHandler action = raffleizeTransactionHandler (RaffleizeOffchainContext validatorsTxOutRefs providersCtx) secretKey action (Just contextNFT) Nothing s (actionToLabel action `elem` tiAvailableActions selectedTicketInfo)
              case key of
                (KChar 's') -> if ("TicketOwner", "RevealTicketSecret") `elem` tiAvailableActions selectedTicketInfo then put $ s {currentScreen = RevealTicketSecretScreen} else put s
                (KChar 'w') -> actionHandler (TicketOwner CollectStake)
                (KChar 'r') -> actionHandler (TicketOwner RefundTicket)
                (KChar 'e') -> actionHandler (TicketOwner RefundTicketExtra)
                (KChar 'l') -> actionHandler (TicketOwner RefundCollateralLosing)
                _ -> do
                  (newMtForm, ()) <- nestEventM myTicketsForm $ handleFormEvent event
                  put $ s {myTicketsForm = newMtForm}
            Nothing -> do
              (newMtForm, ()) <- nestEventM myTicketsForm $ handleFormEvent event
              put $ s {myTicketsForm = newMtForm}
        _ -> do
          (newMtForm, ()) <- nestEventM myTicketsForm $ handleFormEvent event
          put $ s {myTicketsForm = newMtForm}
handleMyTicketsEvents _ _ = error "Invalid use of handleMyTicketsEvents"

formStateFromRaffleConfig :: RaffleConfig -> RaffleConfigFormState
formStateFromRaffleConfig RaffleConfig {..} = do
  let commitSuggestion = Data.Text.pack . gyIso8601Show $ timeFromPlutus rCommitDDL
  let revealSuggestion = Data.Text.pack . gyIso8601Show $ timeFromPlutus rRevealDDL
  RaffleConfigFormState commitSuggestion revealSuggestion (fromIntegral rTicketPrice `div` 1000000) (fromIntegral rMinTickets) mempty

-- | Handle events in My Tickets Screen
handleMyRafflesEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleMyRafflesEvents s@RaffleizeUI {..} event
  | currentScreen == MyRafflesScreen = do
      let mrFormState = formState myRafflesForm
          maybeSelectedRaffle = mrFormState ^. selectedMyRaffle
       in case (event, maybeSecretKey, validatorsConfig) of
            (VtyEvent (EvKey key _modifiers), Just secretKey, Just validatorsTxOutRefs) ->
              case maybeSelectedRaffle of
                Just selectedRaffleInfo -> do
                  let contextNFT = rRaffleID $ riRsd selectedRaffleInfo
                      actionHandler action = raffleizeTransactionHandler (RaffleizeOffchainContext validatorsTxOutRefs providersCtx) secretKey action (Just contextNFT) Nothing s (actionToLabel action `elem` riAvailableActions selectedRaffleInfo)
                  case key of
                    (KChar 'u') ->
                      if ("RaffleOwner", "Update") `elem` riAvailableActions selectedRaffleInfo
                        then do
                          let selectedRaffleConfig = rConfig $ riRsd selectedRaffleInfo
                          let newConstructedValueState = mkConstructValueState balance (rStake selectedRaffleConfig)
                          let newRaffleConfigForm = updateFormState (formStateFromRaffleConfig selectedRaffleConfig) raffleConfigForm
                          put $ s {currentScreen = UpdateRaffleScreen, raffleConfigForm = newRaffleConfigForm, myConstrctValueState = newConstructedValueState, updatingRaffle = Just selectedRaffleInfo}
                        else put s
                    (KChar 'c') -> actionHandler (RaffleOwner Cancel)
                    (KChar 'r') -> actionHandler (RaffleOwner RecoverStake)
                    (KChar 'e') -> actionHandler (RaffleOwner RecoverStakeAndAmount)
                    (KChar 'a') -> actionHandler (RaffleOwner CollectAmount)
                    _ -> do
                      (newFormState, ()) <- nestEventM myRafflesForm $ handleFormEvent event
                      put $ s {myRafflesForm = newFormState}
                Nothing -> do
                  (newFormState, ()) <- nestEventM myRafflesForm $ handleFormEvent event
                  put $ s {myRafflesForm = newFormState}
            _ -> do
              (newFormState, ()) <- nestEventM myRafflesForm $ handleFormEvent event
              put $ s {myRafflesForm = newFormState}
handleMyRafflesEvents _ _ = error "Invalid use of handleMyRafflesEvents"

-- | Handle events in Mint Tokens Screen
handleMintTokensEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleMintTokensEvents s@RaffleizeUI {currentScreen, maybeSecretKey, mintTokenForm, providersCtx} event
  | currentScreen == MintTokenScreen =
      case (event, maybeSecretKey) of
        (VtyEvent (EvKey key _modifiers), Just secretKey) ->
          case (key, allFieldsValid mintTokenForm) of
            (KEnter, True) ->
              do
                liftIO clearScreen
                let mtFormState = formState mintTokenForm
                txOutRef <-
                  liftIO $
                    mintTestTokens
                      providersCtx
                      secretKey
                      (Data.Text.unpack (mtFormState ^. tokenNameField))
                      (fromIntegral (mtFormState ^. mintAmount))
                let nid = (cfgNetworkId . ctxCoreCfg) providersCtx
                initialState <- liftIO $ buildInitialState providersCtx (logo s)
                put $
                  initialState
                    { message = "TEST TOKENS SUCCESSFULLY MINTED !\n" <> showLink nid "tx" txOutRef
                    }
            _ -> do
              (newMtForm, ()) <- nestEventM mintTokenForm $ handleFormEvent event
              let newMtFormState = formState newMtForm
              let fieldValidations =
                    [ setFieldValid (Data.Text.length (newMtFormState ^. tokenNameField) <= tokenNameMaxLength) TokenNameField,
                      setFieldValid (newMtFormState ^. mintAmount > 0) MintAmountField
                    ]
              let validatedForm = foldr' ($) newMtForm fieldValidations
              put $ s {mintTokenForm = validatedForm}
        _ -> put s
handleMintTokensEvents _state _event = error "Invalid use of handleMintTokensEvents"

-- | Handle events in Mint Tokens Screen
handleMnemonicEvents :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources RaffleizeUI ()
handleMnemonicEvents s@RaffleizeUI {currentScreen, mnemonicForm, providersCtx} event
  | currentScreen == ImportWalletFromMnemonic =
      case event of
        (VtyEvent (EvKey KEnter _modifiers)) -> do
          liftIO clearScreen
          let mnFormState = formState mnemonicForm
          liftIO $ Data.Text.IO.writeFile operationPrivFilePath (_mnemonicField mnFormState)
          initialState <- liftIO $ buildInitialState providersCtx (logo s)
          put $
            initialState
              { message = "WALLET IMPORTED SUCCESSFULLY !\n" <> showText operationPrivFilePath
              }
        _ -> do
          (newMnForm, ()) <- nestEventM mnemonicForm $ handleFormEvent event
          let newMnFormState = formState newMnForm
          let fieldValidations =
                [ setFieldValid (isValidMnemonic (newMnFormState ^. mnemonicField)) MnemonicField
                ]
          let validatedForm = foldr' ($) newMnForm fieldValidations
          put $ s {mnemonicForm = validatedForm}
handleMnemonicEvents _state _event = error "Invalid use of handleMintTokensEvents"

------------------------------------------------------------------------------------------------

-- *  Drawing

------------------------------------------------------------------------------------------------

drawUI :: RaffleizeUI -> [Widget NameResources]
drawUI s =
  let maybeSelectedRaffleId = rRaffleID . riRsd <$> formState (activeRafflesForm s) ^. selectedRaffle
      maybeUpdatingRaffleId = rRaffleID . riRsd <$> updatingRaffle s
      maybeSelectedTicket = liftA2 (,) tRaffle tNumber . tiTsd <$> formState (myTicketsForm s) ^. selectedTicket
   in joinBorders . withBorderStyle unicode . borderWithLabel (txt " RAFFLEIZE - C.A.R.D.A.N.A ")
        <$> [ if Data.Text.null (message s) then emptyWidget else center (withAttr (attrName "highlight") $ txt (message s)) <=> txt "[ESC] - Close",
              if currentScreen s == MintTokenScreen then drawMintTestTokensForm (mintTokenForm s) else emptyWidget,
              if currentScreen s == CreateRaffleScreen then drawCreateRaffleForm (raffleConfigForm s) (myConstrctValueState s) else emptyWidget,
              if currentScreen s == UpdateRaffleScreen then drawCreateUpdateRaffleForm maybeUpdatingRaffleId (raffleConfigForm s) (myConstrctValueState s) else emptyWidget,
              if currentScreen s == BuyTicketScreen then drawBuyTicketForm maybeSelectedRaffleId (buyTicketForm s) else emptyWidget,
              if currentScreen s == ActiveRafflesScreen then drawActiveRafflesForm (activeRafflesForm s) else emptyWidget,
              if currentScreen s == MyRafflesScreen then drawMyRafflesForm (myRafflesForm s) else emptyWidget,
              if currentScreen s == MyTicketsScreen then drawMyTicketsScreen (myTicketsForm s) else emptyWidget,
              if currentScreen s == ConstructValueScreen then drawConstructValueWidget (myConstrctValueState s) else emptyWidget,
              if currentScreen s == RevealTicketSecretScreen then drawRevealTicketSecretScreen maybeSelectedTicket (revealSecretForm s) else emptyWidget,
              if currentScreen s == ImportWalletFromMnemonic then drawMnemonicForm (mnemonicForm s) else emptyWidget,
              mainScreen s
            ]

------------------------------------------------------------------------------------------------

-- **  Main Screen

------------------------------------------------------------------------------------------------

mainScreen :: RaffleizeUI -> Widget NameResources
mainScreen s =
  vBox
    [ hCenter $ withAttr (attrName "highlight") $ str (logo s),
      hBorder,
      center bodyWidget
    ]
  where
    bodyWidget :: Widget NameResources
    bodyWidget =
      vBox $
        padLeftRight 1
          <$> [ summaryWidget,
                hBorder,
                assetsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (maybeSecretKey s) (balanceList s)
              ]
      where
        summaryWidget :: Widget NameResources
        summaryWidget =
          hCenter $
            hBox
              [ availableActionsWidget,
                providersWidget ((ctxCoreCfg . providersCtx) s),
                validatorsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (validatorsConfig s),
                walletFlagWidget (maybeSecretKey s)
              ]
          where
            validatorsWidget :: GYNetworkId -> Maybe RaffleizeTxBuildingContext -> Widget n
            validatorsWidget nid mv =
              borderWithLabel (txt "VALIDATORS") $
                renderTable $
                  table $
                    [txt "Deployed: ", symbolWidget (isJust mv)] : case mv of
                      (Just (RaffleizeTxBuildingContext {..})) ->
                        [ [txt "Raffle Validator", txOutRefWidget nid raffleValidatorRef],
                          [txt "Ticket Validator", txOutRefWidget nid ticketValidatorRef],
                          [txt "Minting Policy", txOutRefWidget nid mintingPolicyRef]
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
                    [ [txt "Loaded: ", symbolWidget True],
                      [txt "Provider: ", printProvider cfg],
                      [txt "Network: ", printNetwork cfg]
                    ]
            availableActionsWidget :: Widget n
            availableActionsWidget =
              withAttr (attrName "action") . borderWithLabel (txt "AVAILABLE ACTIONS") $
                vBox
                  ( txt
                      <$> filter
                        (not . Data.Text.null)
                        ( ( if isNothing (maybeSecretKey s)
                              then ["[I] - Import wallet from mnemonic"]
                              else
                                [ "[V] - View all active raffles",
                                  "[R] - View my raffles",
                                  "[T] - View my tickets",
                                  "[C] - Create raffle", -- TODO : CHECK ADA BALANCE
                                  "[M] - Mint some test tokens",
                                  "[D] - Deploy Raffleize Validators",
                                  "[L] - Refresh wallet balance"
                                ]
                          )
                            ++ [ "[E] - Export validators",
                                 "[Q] - Quit"
                               ]
                        )
                  )
    assetsWidget :: GYNetworkId -> Maybe GYExtendedPaymentSigningKey -> GenericList NameResources [] (CurrencySymbol, TokenName, Integer) -> Widget NameResources
    assetsWidget nid (Just skey) valList =
      let addr = addressFromPaymentSigningKey nid skey
          val = unFlattenValue $ listElements valList
       in borderWithLabel (txt "WALLET") $
            hBox
              [ assetsAdaWidget addr val,
                vBorder,
                assetsBalanceWidget valList
              ]
      where
        assetsAdaWidget :: GYAddress -> Value -> Widget NameResources
        assetsAdaWidget addr val =
          renderTable $
            table
              [ [txt "Address: ", addressWidget],
                [txt "Ada | ₳ |:", adaBalanceWidget]
              ]
          where
            addressWidget :: Widget n
            addressWidget =
              let addrText = addressToText addr
               in hyperlink (showLink nid "address" addrText) $ withAttr (attrName "good") $ txt addrText
            adaBalanceWidget :: Widget n
            adaBalanceWidget = withAttr (attrName "good") $ txt (showText (fromValue val) <> "\n")
    assetsWidget _ _ _ = emptyWidget
    assetsBalanceWidget :: (Traversable t, Splittable t) => GenericList NameResources t (CurrencySymbol, TokenName, Integer) -> Widget NameResources
    assetsBalanceWidget valueItemsList =
      vLimit 100 $
        hLimit 150 $
          borderWithLabel (txt "ASSETS") $
            renderList (valueItemWidget True) False valueItemsList

------------------------------------------------------------------------------------------------

-- **  Styling

------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    (white `on` black)
    [ (attrName "highlight", fg magenta),
      (attrName "warning", fg red),
      (attrName "good", fg green),
      (attrName "action", fg yellow),
      (attrName "selected", bg blue),
      (focusedFormInputAttr, fg brightBlue),
      (invalidFormInputAttr, white `on` red)
    ]

---------

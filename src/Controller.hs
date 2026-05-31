{-# LANGUAGE OverloadedStrings #-}

module Controller (runMenu) where

import Finance
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Database.PostgreSQL.Simple
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import System.IO (hFlush, stdout)
import Lens.Micro (Lens', lens)
import Lens.Micro.Mtl (zoom)

-- ── Names ────────────────────────────────────────────────────

data Name = MenuList | ResultList deriving (Eq, Ord, Show)

-- ── Data types ───────────────────────────────────────────────

data ResultItem = ResultItem
  { riText     :: String
  , riFixLabel :: String
  , riFix      :: Maybe (IO ())
  }

data MenuItem = MenuItem
  { miLabel   :: String
  , miHeader  :: String
  , miCount   :: Int
  , miLoad    :: IO [ResultItem]
  , miCountQ  :: IO Int
  }

data AppState = AppState
  { _db           :: Connection
  , _menuList     :: BL.List Name MenuItem
  , _resultList   :: BL.List Name ResultItem
  , _curLoad      :: IO [ResultItem]
  , _curLabel     :: String
  , _curMenuIdx   :: Maybe Int
  , _resultHeader :: String
  , _status       :: String
  , _focusLeft    :: Bool
  }

-- ── Formatting helpers ───────────────────────────────────────

padR :: Int -> String -> String
padR n s = let s' = take n s in s' ++ replicate (max 0 (n - length s')) ' '

fmtId :: Integer -> String
fmtId i = padR 9 ("[" ++ show i ++ "]") ++ " "

fmtTx :: Transaction -> String
fmtTx t =
  fmtId (transactionTransactionId t) ++
  show (transactionTransactionDate t) ++ "  " ++
  padR 35 (transactionDescription t) ++ "  " ++
  padR 25 (transactionAccountNameOwner t) ++ "  " ++
  padR 12 (transactionTransactionState t) ++ "  " ++
  padR 10 (show (transactionAmount t)) ++ "  " ++
  transactionCategory t

fmtAccount :: Account -> String
fmtAccount a =
  padR 35 (accountAccountNameOwner a) ++ "  " ++
  padR 10 (accountAccountType a) ++ "  " ++
  "outstanding: " ++ show (accountOutstanding a)

fmtDescription :: Description -> String
fmtDescription d =
  fmtId (descriptionId d) ++
  padR 40 (descriptionName d) ++ "  " ++
  "owner: " ++ descriptionOwner d

fmtCategory :: Category -> String
fmtCategory c =
  fmtId (categoryId c) ++
  padR 40 (categoryName c) ++ "  " ++
  "owner: " ++ categoryOwner c

fmtPayment :: Payment -> String
fmtPayment p =
  fmtId (paymentId p) ++
  show (paymentTransactionDate p) ++ "  " ++
  padR 25 (paymentSourceAccount p) ++ "  " ++
  padR 25 (paymentDestinationAccount p) ++ "  " ++
  show (paymentAmount p)

fmtTransfer :: Transfer -> String
fmtTransfer t =
  fmtId (transferId t) ++
  show (transferTransactionDate t) ++ "  " ++
  padR 25 (transferSourceAccount t) ++ "  " ++
  padR 25 (transferDestinationAccount t) ++ "  " ++
  show (transferAmount t)

fmtPendingTx :: PendingTransaction -> String
fmtPendingTx p =
  fmtId (pendingTransactionId p) ++
  show (pendingTransactionDate p) ++ "  " ++
  padR 28 (pendingTransactionAccountNameOwner p) ++ "  " ++
  padR 30 (pendingTransactionDescription p) ++ "  " ++
  padR 10 (show (pendingTransactionAmount p)) ++ "  " ++
  pendingTransactionReviewStatus p

-- ── Column headers ───────────────────────────────────────────
-- 4-space prefix aligns with the "> " and "* " row markers in renderResultRow

txHeader :: String
txHeader =
  "    " ++
  padR 10 "ID" ++
  padR 12 "Date" ++
  padR 37 "Description" ++
  padR 27 "Account" ++
  padR 14 "State" ++
  padR 12 "Amount" ++
  "Category"

acctHeader :: String
acctHeader =
  "    " ++
  padR 37 "Account" ++
  padR 12 "Type" ++
  "Outstanding"

descHeader :: String
descHeader =
  "    " ++
  padR 10 "ID" ++
  padR 42 "Name" ++
  "Owner"

paymentHeader :: String
paymentHeader =
  "    " ++
  padR 10 "ID" ++
  padR 12 "Date" ++
  padR 27 "Source" ++
  padR 27 "Destination" ++
  "Amount"

pendingTxHeader :: String
pendingTxHeader =
  "    " ++
  padR 10 "ID" ++
  padR 12 "Date" ++
  padR 30 "Account" ++
  padR 32 "Description" ++
  padR 12 "Amount" ++
  "Status"

-- ── Report definition helpers ────────────────────────────────

type ReportDef = (String, String, IO Int, IO [ResultItem])
--                label   header  countQ   loader

mkReadOnly :: String -> String -> IO [a] -> (a -> String) -> ReportDef
mkReadOnly label header q fmt =
  ( label
  , header
  , length <$> q
  , map (\x -> ResultItem (fmt x) "" Nothing) <$> q
  )

mkFixable :: String -> String -> IO [a] -> (a -> String) -> (a -> IO ()) -> String -> ReportDef
mkFixable label header q fmt fixFn fixLabel =
  ( label
  , header
  , length <$> q
  , map (\x -> ResultItem (fmt x) fixLabel (Just (fixFn x))) <$> q
  )

-- ── Report definitions ───────────────────────────────────────

reportDefs :: Connection -> [ReportDef]
reportDefs c =
  [ mkFixable "Transactions with double spaces"
      txHeader (selectTransactionsWithDoubleSpaces c) fmtTx
      (updateTransactionDescriptionFixDoubleSpaces c) "Fix"

  , mkFixable "Orphaned descriptions"
      descHeader (selectOrphanedDescriptions c) fmtDescription
      (\d -> deleteOrphanedDescription c (descriptionId d)) "Delete"

  , mkFixable "Orphaned categories"
      descHeader (selectOrphanedCategories c) fmtCategory
      (\cat -> deleteOrphanedCategory c (categoryId cat)) "Delete"

  , mkReadOnly "Orphaned receipt images"
      ("    " ++ padR 10 "ID" ++ padR 12 "Tx ID" ++ padR 22 "Owner" ++ "Format")
      (selectOrphanedReceiptImages c)
      (\r -> fmtId (receiptImageId r) ++
             padR 10 (show (receiptImageTransactionId r)) ++ "  " ++
             padR 20 (receiptImageOwner r) ++ "  " ++
             receiptImageFormatType r)

  , mkReadOnly "Descriptions used > 10 times"
      ("    " ++ padR 8 "Count" ++ "  " ++ "Description")
      (selectDescriptionsUsedMoreThanTenTimes c)
      (\d -> padR 8 (show (descriptionCount d)) ++ "  " ++ descriptionCountName d)

  , mkReadOnly "Cleared count by week"
      ("    " ++ padR 12 "Week Start" ++ "Count")
      (selectClearedTransactionCountByWeek c)
      (\w -> padR 12 (show (weekStart w)) ++ show (clearedCount w))

  , mkReadOnly "Cleared count by month"
      ("    " ++ padR 12 "Month Start" ++ "Count")
      (selectClearedTransactionCountByMonth c)
      (\m -> padR 12 (show (monthStart m)) ++ show (monthlyClearedCount m))

  , mkFixable "Stale outstanding/future transactions"
      txHeader (selectStaleOutstandingTransactions c) fmtTx
      (\t -> updateTransactionStateToCleared c (transactionTransactionId t)) "Mark cleared"

  , mkReadOnly "Cleared transactions with future date"
      txHeader (selectClearedFutureDateTransactions c) fmtTx

  , mkReadOnly "Active txns on inactive accounts"
      txHeader (selectActiveTransactionsOnInactiveAccounts c) fmtTx

  , mkReadOnly "Uncategorized transactions"
      txHeader (selectUncategorizedTransactions c) fmtTx

  , mkReadOnly "Transactions with undefined type"
      txHeader (selectUndefinedTransactionType c) fmtTx

  , mkFixable "Zero-amount transactions"
      txHeader (selectZeroAmountTransactions c) fmtTx
      (\t -> deactivateTransaction c (transactionTransactionId t)) "Deactivate"

  , mkFixable "Zero-amount inactive transactions"
      txHeader (selectZeroAmountInactiveTransactions c) fmtTx
      (deleteTransactionCascade c) "Delete"

  , mkFixable "Zero-amount inactive on active accounts"
      txHeader (selectZeroAmountInactiveTransactionsOnActiveAccounts c) fmtTx
      (deleteTransactionCascade c) "Delete"

  , mkReadOnly "Dangling payments"
      paymentHeader (selectDanglingPayments c) fmtPayment

  , mkReadOnly "Dangling transfers"
      paymentHeader (selectDanglingTransfers c) fmtTransfer

  , mkReadOnly "Transactions with mismatched account type"
      txHeader (selectTransactionsWithMismatchedAccountType c) fmtTx

  , mkReadOnly "Accounts never validated"
      acctHeader (selectAccountsNeverValidated c) fmtAccount

  , mkReadOnly "Active accounts with no transactions"
      acctHeader (selectAccountsWithNoTransactions c) fmtAccount

  , mkReadOnly "Descriptions with leading/trailing spaces"
      descHeader (selectDescriptionsWithWhitespace c)
      (\d -> fmtId (descriptionId d) ++
             "|" ++ padR 40 (descriptionName d) ++ "|  " ++
             "owner: " ++ descriptionOwner d)

  , mkReadOnly "Pending transactions older than 30 days"
      pendingTxHeader (selectPendingTransactionsOlderThan30Days c) fmtPendingTx

  , mkReadOnly "Account totals out of sync"
      ("    " ++ padR 37 "Account" ++ padR 22 "Cleared(stored/actual)" ++ padR 24 "Outstanding(stored/actual)" ++ "Future(stored/actual)")
      (selectAccountTotalsOutOfSync c)
      (\r -> padR 35 (atdAccountNameOwner r) ++ "  " ++
             padR 20 (show (atdStoredCleared r) ++ "/" ++ show (atdComputedCleared r)) ++ "  " ++
             padR 22 (show (atdStoredOutstanding r) ++ "/" ++ show (atdComputedOutstanding r)) ++ "  " ++
             show (atdStoredFuture r) ++ "/" ++ show (atdComputedFuture r))

  , mkReadOnly "Account validation date out of sync"
      ("    " ++ padR 37 "Account" ++ padR 30 "Stored Date" ++ "Actual Date")
      (selectAccountValidationDateOutOfSync c)
      (\r -> padR 35 (avmAccountNameOwner r) ++ "  " ++
             padR 28 (show (avmStoredValidationDate r)) ++ "  " ++
             show (avmActualValidationDate r))

  , mkReadOnly "Inactive descriptions still used"
      descHeader (selectInactiveDescriptionsStillUsed c) fmtDescription

  , mkReadOnly "Inactive categories still used"
      descHeader (selectInactiveCategoriesStillUsed c) fmtCategory

  , mkReadOnly "Transactions with negative amount"
      txHeader (selectTransactionsWithNegativeAmount c) fmtTx

  , mkReadOnly "Transactions with unreasonable date"
      txHeader (selectTransactionsWithUnreasonableDate c) fmtTx

  , mkReadOnly "Orphaned transaction-category links"
      ("    " ++ padR 16 "Category ID" ++ padR 16 "Tx ID" ++ "Owner")
      (selectOrphanedTransactionCategories c)
      (\r -> padR 14 (show (tcCategoryId r)) ++ "  " ++
             padR 14 (show (tcTransactionId r)) ++ "  " ++
             tcOwner r)

  , mkReadOnly "Self-referencing payments"
      paymentHeader (selectSelfReferencingPayments c) fmtPayment

  , mkReadOnly "Self-referencing transfers"
      paymentHeader (selectSelfReferencingTransfers c) fmtTransfer

  , mkReadOnly "Unpaid medical expenses"
      ("    " ++ padR 10 "ID" ++ padR 12 "Date" ++ padR 14 "Billed" ++ padR 14 "Patient" ++ padR 12 "Paid" ++ "Status")
      (selectUnpaidMedicalExpenses c)
      (\m -> fmtId (medicalExpenseId m) ++
             padR 10 (show (medicalExpenseServiceDate m)) ++ "  " ++
             padR 12 (show (medicalExpenseBilledAmount m)) ++ "  " ++
             padR 12 (show (medicalExpensePatientResponsibility m)) ++ "  " ++
             padR 10 (show (medicalExpensePaidAmount m)) ++ "  " ++
             medicalExpenseClaimStatus m)
  ]

buildMenuItems :: Connection -> IO [MenuItem]
buildMenuItems conn = forM (reportDefs conn) $ \(label, header, countQ, loader) -> do
  n <- countQ
  return (MenuItem label header n loader countQ)

-- ── Lenses ───────────────────────────────────────────────────

menuListL :: Lens' AppState (BL.List Name MenuItem)
menuListL = lens _menuList (\s l -> s { _menuList = l })

resultListL :: Lens' AppState (BL.List Name ResultItem)
resultListL = lens _resultList (\s l -> s { _resultList = l })

-- ── TUI app ──────────────────────────────────────────────────

app :: App AppState e Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const theAttrMap
  }

headerAttr :: AttrName
headerAttr = attrName "header"

theAttrMap :: AttrMap
theAttrMap = attrMap V.defAttr
  [ (BL.listSelectedFocusedAttr, V.withStyle V.defAttr V.reverseVideo)
  , (BL.listSelectedAttr,        V.withStyle V.defAttr V.underline)
  , (headerAttr,                 V.withStyle V.defAttr V.bold)
  ]

drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui =
      withBorderStyle unicodeRounded $
      vBox
        [ hBox [menuPanel, vBorder, resultPanel]
        , hBorder
        , helpBar
        ]

    menuPanel =
      hLimit 46 $
      borderWithLabel (str " Finance Reports ") $
      BL.renderList renderMenuRow (_focusLeft st) (_menuList st)

    resultPanel =
      borderWithLabel (str (" " ++ titleLine ++ " ")) $
      let hdr = _resultHeader st
          headerWidget
            | null hdr  = emptyWidget
            | otherwise = vBox
                [ withAttr headerAttr (str hdr)
                , hBorder
                ]
          bodyWidget
            | Vec.null (BL.listElements (_resultList st)) =
                center (str "Press Enter on a report to load results")
            | otherwise =
                BL.renderList renderResultRow (not (_focusLeft st)) (_resultList st)
      in vBox [headerWidget, bodyWidget]

    titleLine =
      let n = Vec.length (BL.listElements (_resultList st))
      in if null (_curLabel st)
           then "Select a report"
           else _curLabel st ++ " (" ++ show n ++ ")"

    renderMenuRow focused item =
      let mark  = if focused then ">" else " "
          badge = "(" ++ show (miCount item) ++ ")"
      in str (mark ++ " " ++ padR 38 (miLabel item) ++ " " ++ badge)

    renderResultRow focused ri =
      let mark     = if focused then "> " else "  "
          fixMark  = case riFix ri of
            Just _  -> "* "
            Nothing -> "  "
      in str (mark ++ fixMark ++ riText ri)

    helpBar =
      padLeft (Pad 1) $
      if not (null (_status st))
        then str (_status st)
        else case (not (_focusLeft st), BL.listSelectedElement (_resultList st)) of
          (True, Just (_, ri)) | Just _ <- riFix ri ->
            str ("←:menu  ↑↓:navigate  f:" ++ riFixLabel ri ++ "  r:refresh  q:quit")
          _ ->
            str "←:menu  →:results  ↑↓:navigate  Enter:load  f:fix  r:refresh  q:quit"

-- ── Event handling ───────────────────────────────────────────

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) =
  modify (\s -> s { _focusLeft = not (_focusLeft s), _status = "" })
handleEvent (VtyEvent (V.EvKey V.KLeft [])) =
  modify (\s -> s { _focusLeft = True, _status = "" })
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  st <- get
  if _focusLeft st
    then modify (\s -> s { _focusLeft = False, _status = "" })
    else zoom resultListL (BL.handleListEvent (V.EvKey V.KRight []))
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  st <- get
  let pageDown = V.EvKey V.KPageDown []
  if _focusLeft st
    then zoom menuListL   (BL.handleListEvent pageDown)
    else zoom resultListL (BL.handleListEvent pageDown)
  modify (\s -> s { _status = "" })
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = loadReport
handleEvent (VtyEvent (V.EvKey (V.KChar 'f') [])) = applyFix
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = refreshResults
handleEvent (VtyEvent ve) = do
  st <- get
  if _focusLeft st
    then zoom menuListL (BL.handleListEvent ve)
    else zoom resultListL (BL.handleListEvent ve)
  modify (\s -> s { _status = "" })
handleEvent _ = return ()

loadReport :: EventM Name AppState ()
loadReport = do
  st <- get
  case BL.listSelectedElement (_menuList st) of
    Nothing -> return ()
    Just (idx, item) -> do
      result <- liftIO $ try (miLoad item)
      case result of
        Left err ->
          modify (\s -> s { _status = "Error: " ++ show (err :: SomeException) })
        Right items -> do
          let n           = length items
              newResults  = BL.list ResultList (Vec.fromList items) 1
              updatedItem = item { miCount = n }
              newElems    = Vec.update (BL.listElements (_menuList st))
                              (Vec.singleton (idx, updatedItem))
              newMenuList = BL.listReplace newElems (Just idx) (_menuList st)
          modify (\s -> s
            { _resultList   = newResults
            , _curLoad      = miLoad item
            , _curLabel     = miLabel item
            , _curMenuIdx   = Just idx
            , _resultHeader = miHeader item
            , _menuList     = newMenuList
            , _status       = ""
            , _focusLeft    = False
            })

applyFix :: EventM Name AppState ()
applyFix = do
  st <- get
  case BL.listSelectedElement (_resultList st) of
    Nothing -> return ()
    Just (oldIdx, ri) ->
      case riFix ri of
        Nothing ->
          modify (\s -> s { _status = "No fix available for this item" })
        Just fixAction -> do
          result <- liftIO $ try fixAction
          case result of
            Left err ->
              modify (\s -> s { _status = "Fix failed: " ++ show (err :: SomeException) })
            Right () -> do
              items <- liftIO (_curLoad st)
              let n          = length items
                  newIdx     = min oldIdx (max 0 (n - 1))
                  newResults = BL.listMoveTo newIdx $
                                 BL.list ResultList (Vec.fromList items) 1
              case _curMenuIdx st of
                Nothing -> modify (\s -> s
                  { _resultList = newResults
                  , _status     = "Fixed. " ++ show n ++ " item(s) remaining."
                  })
                Just menuIdx ->
                  case BL.listElements (_menuList st) Vec.!? menuIdx of
                    Nothing -> return ()
                    Just mitem -> do
                      let updatedMItem = mitem { miCount = n }
                          newElems     = Vec.update (BL.listElements (_menuList st))
                                           (Vec.singleton (menuIdx, updatedMItem))
                          newMenuList  = BL.listReplace newElems (Just menuIdx) (_menuList st)
                      modify (\s -> s
                        { _resultList = newResults
                        , _menuList   = newMenuList
                        , _status     = "Fixed. " ++ show n ++ " item(s) remaining."
                        })

refreshResults :: EventM Name AppState ()
refreshResults = do
  st <- get
  result <- liftIO $ try (_curLoad st)
  case result of
    Left err ->
      modify (\s -> s { _status = "Refresh failed: " ++ show (err :: SomeException) })
    Right items -> do
      let newResults = BL.list ResultList (Vec.fromList items) 1
      modify (\s -> s { _resultList = newResults, _status = "Refreshed." })

-- ── Entry point ──────────────────────────────────────────────

runMenu :: Connection -> IO ()
runMenu conn = do
  putStr "Loading report counts..."
  hFlush stdout
  items <- buildMenuItems conn
  putStrLn " done."
  let menuL     = BL.list MenuList (Vec.fromList items) 1
      emptyRes  = BL.list ResultList Vec.empty 1
      initState = AppState
        { _db           = conn
        , _menuList     = menuL
        , _resultList   = emptyRes
        , _curLoad      = return []
        , _curLabel     = ""
        , _curMenuIdx   = Nothing
        , _resultHeader = ""
        , _status       = ""
        , _focusLeft    = True
        }
  void $ defaultMain app initState

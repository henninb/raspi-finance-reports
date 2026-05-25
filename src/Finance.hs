{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Finance (lookupEnv, sumOfTransactions, extractCategories,
                outstandingTransactions, futureTransactions, sortAndGroupByList, transactionDebits,
                selectAllTransactions, selectAllAccounts, selectAllCategories,
                selectTransactionsWithDoubleSpaces, updateTransactionDescriptionFixDoubleSpaces,
                selectClearedTransactionCountByWeek, selectClearedTransactionCountByMonth,
                selectDescriptionsUsedMoreThanTenTimes,
                selectOrphanedCategories, deleteOrphanedCategory, selectOrphanedDescriptions, deleteOrphanedDescription, selectOrphanedReceiptImages,
                selectStaleOutstandingTransactions, updateTransactionStateToCleared, selectClearedFutureDateTransactions,
                selectActiveTransactionsOnInactiveAccounts,
                selectUncategorizedTransactions, selectUndefinedTransactionType,
                selectZeroAmountTransactions, selectZeroAmountInactiveTransactions, selectZeroAmountInactiveTransactionsOnActiveAccounts, deactivateTransaction, deleteTransaction, deleteTransactionCascade,
                selectDanglingPayments, selectDanglingTransfers,
                selectTransactionsWithMismatchedAccountType,
                selectAccountsNeverValidated, selectAccountsWithNoTransactions,
                selectDescriptionsWithWhitespace,
                selectPendingTransactionsOlderThan30Days,
                selectAccountTotalsOutOfSync, selectAccountValidationDateOutOfSync,
                selectInactiveDescriptionsStillUsed, selectInactiveCategoriesStillUsed,
                selectTransactionsWithNegativeAmount, selectTransactionsWithUnreasonableDate,
                selectOrphanedTransactionCategories,
                selectSelfReferencingPayments, selectSelfReferencingTransfers,
                selectUnpaidMedicalExpenses,
                transactionCredits, someUUIDs, isCleared,
                sumOfActiveTransactions, hasTransactionId, findByTransactionId,
                Transaction(..), Account(..), Category(..), Description(..), ReceiptImage(..),
                WeeklyClearedCount(..), MonthlyClearedCount(..), DescriptionCount(..),
                Payment(..), Transfer(..), PendingTransaction(..),
                AccountTotalsDiscrepancy(..), AccountValidationMismatch(..),
                TransactionCategory(..), MedicalExpense(..)) where

import Data.Aeson
import Data.Time
import Control.Monad
import Control.Arrow
import Data.List
import Database.PostgreSQL.Simple
import GHC.Generics
import Data.Scientific
import System.Environment
import Data.UUID
import System.Random

data Category = Category
    { categoryName        :: String
    , categoryId          :: Integer
    , categoryOwner       :: String
    , categoryActiveStatus :: Bool
    } deriving (Show, Generic, Eq, ToJSON, FromJSON)

instance FromRow Category
instance ToRow Category

data Account = Account
    { accountAccountNameOwner :: String
    , accountAccountId        :: Integer
    , accountAccountType      :: String
    , accountActiveStatus     :: Bool
    , accountMoniker          :: String
    , accountOwner            :: String
    , accountOutstanding      :: Scientific
    , accountFuture           :: Scientific
    , accountCleared          :: Scientific
    } deriving (Show, Eq, Generic, Ord, ToJSON, FromJSON)

instance FromRow Account
instance ToRow Account

data Transaction = Transaction
    { transactionGuid                :: String
    , transactionDescription         :: String
    , transactionCategory            :: String
    , transactionAccountType         :: String
    , transactionAccountNameOwner    :: String
    , transactionNotes               :: String
    , transactionTransactionState    :: String
    , transactionAccountId           :: Integer
    , transactionTransactionId       :: Integer
    , transactionReoccurringType     :: String
    , transactionActiveStatus        :: Bool
    , transactionTransactionDate     :: Day
    , transactionAmount              :: Scientific
    , transactionOwner               :: String
    , transactionTransactionType     :: String
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance FromRow Transaction
instance ToRow Transaction

data WeeklyClearedCount = WeeklyClearedCount
    { weekStart    :: Day
    , clearedCount :: Integer
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow WeeklyClearedCount
instance ToRow WeeklyClearedCount

data MonthlyClearedCount = MonthlyClearedCount
    { monthStart          :: Day
    , monthlyClearedCount :: Integer
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow MonthlyClearedCount
instance ToRow MonthlyClearedCount

data DescriptionCount = DescriptionCount
    { descriptionCountName :: String
    , descriptionCount     :: Integer
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow DescriptionCount
instance ToRow DescriptionCount

data Description = Description
    { descriptionName         :: String
    , descriptionId           :: Integer
    , descriptionOwner        :: String
    , descriptionActiveStatus :: Bool
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow Description
instance ToRow Description

data ReceiptImage = ReceiptImage
    { receiptImageId            :: Integer
    , receiptImageOwner         :: String
    , receiptImageTransactionId :: Integer
    , receiptImageActiveStatus  :: Bool
    , receiptImageFormatType    :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow ReceiptImage
instance ToRow ReceiptImage

data Payment = Payment
    { paymentId                 :: Integer
    , paymentSourceAccount      :: String
    , paymentDestinationAccount :: String
    , paymentTransactionDate    :: Day
    , paymentAmount             :: Scientific
    , paymentGuidSource         :: String
    , paymentGuidDestination    :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow Payment
instance ToRow Payment

data Transfer = Transfer
    { transferId                 :: Integer
    , transferSourceAccount      :: String
    , transferDestinationAccount :: String
    , transferTransactionDate    :: Day
    , transferAmount             :: Scientific
    , transferGuidSource         :: String
    , transferGuidDestination    :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow Transfer
instance ToRow Transfer

data PendingTransaction = PendingTransaction
    { pendingTransactionId               :: Integer
    , pendingTransactionAccountNameOwner :: String
    , pendingTransactionDate             :: Day
    , pendingTransactionDescription      :: String
    , pendingTransactionAmount           :: Scientific
    , pendingTransactionReviewStatus     :: String
    , pendingTransactionOwner            :: String
    , pendingTransactionDateAdded        :: LocalTime
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow PendingTransaction
instance ToRow PendingTransaction

someUUIDs :: [UUID]
someUUIDs =
  let seed = 137
      g0 = mkStdGen seed
      (u1, g1) = random g0
      (u2, g2) = random g1
      (u3, g3) = random g2
      (u4, _)  = random g3
  in [u1, u2, u3, u4]

sumOfTransactions :: [Transaction] -> Scientific
sumOfTransactions = foldr ((+) . transactionAmount) 0.0

sumOfActiveTransactions :: [Transaction] -> Scientific
sumOfActiveTransactions = sumOfTransactions . filter isActive

futureTransactions :: [Transaction] -> [Transaction]
futureTransactions = filter isFuture

outstandingTransactions :: [Transaction] -> [Transaction]
outstandingTransactions = filter isOutstanding

isOutstanding :: Transaction -> Bool
isOutstanding x = transactionTransactionState x == "outstanding"

hasTransactionId :: Integer -> Transaction -> Bool
hasTransactionId id transaction = transactionTransactionId transaction == id

findByTransactionId :: Integer -> [Transaction] -> Maybe Transaction
findByTransactionId id = find (\f -> transactionTransactionId f == id)

isCleared :: Transaction -> Bool
isCleared x = transactionTransactionState x == "cleared"

isFuture :: Transaction -> Bool
isFuture x = transactionTransactionState x == "future"

isCredit :: Transaction -> Bool
isCredit x = transactionAccountType x == "credit"

isDebit :: Transaction -> Bool
isDebit x = transactionAccountType x == "debit"

isActive :: Transaction -> Bool
isActive = transactionActiveStatus

transactionCredits :: [Transaction] -> [Transaction]
transactionCredits = filter isCredit

transactionDebits :: [Transaction] -> [Transaction]
transactionDebits = filter isDebit

extractCategories :: [Transaction] -> [String]
extractCategories xs = transactionCategory <$> xs

sortAndGroupByList :: Ord a => [a] -> [(a, Int)]
sortAndGroupByList transactions = map (\g@(x:_) -> (x, length g)) $ group $ sort transactions

txCols :: Query
txCols = "guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring_type,active_status,transaction_date,amount,owner,transaction_type"

selectAllTransactions :: Connection -> IO [Transaction]
selectAllTransactions connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE active_status='true'") :: IO [Transaction]

selectAllAccounts :: Connection -> IO [Account]
selectAllAccounts connection = query_ connection
    "SELECT account_name_owner,account_id,account_type,active_status,moniker,owner,outstanding,future,cleared FROM t_account WHERE active_status='true'" :: IO [Account]

selectAllCategories :: Connection -> IO [Category]
selectAllCategories connection = query_ connection
    "SELECT category_name,category_id,owner,active_status FROM t_category WHERE active_status='true'" :: IO [Category]

selectTransactionsWithDoubleSpaces :: Connection -> IO [Transaction]
selectTransactionsWithDoubleSpaces connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE description LIKE '%  %' OR notes LIKE '%  %'") :: IO [Transaction]

updateTransactionDescriptionFixDoubleSpaces :: Connection -> Transaction -> IO ()
updateTransactionDescriptionFixDoubleSpaces connection t = do
    let tid       = transactionTransactionId t
        owner     = transactionOwner t
        badDesc   = transactionDescription t
        fixedDesc = unwords (words badDesc)
    exists <- query connection
        "SELECT 1 FROM t_description WHERE description_name = ? AND owner = ? LIMIT 1"
        (fixedDesc, owner) :: IO [Only Int]
    when (length exists == 0) $ void $
        execute connection
          "UPDATE t_description SET description_name = ? WHERE description_name = ? AND owner = ?"
          (fixedDesc, badDesc, owner)
    void $ execute connection
        "UPDATE t_transaction SET description = ?, notes = regexp_replace(notes, ' {2,}', ' ', 'g') WHERE transaction_id = ?"
        (fixedDesc, tid)

selectClearedTransactionCountByWeek :: Connection -> IO [WeeklyClearedCount]
selectClearedTransactionCountByWeek connection = query_ connection
    "SELECT date_trunc('week', transaction_date)::date, count(*) FROM t_transaction WHERE transaction_state = 'cleared' AND active_status = 'true' GROUP BY date_trunc('week', transaction_date) ORDER BY date_trunc('week', transaction_date)" :: IO [WeeklyClearedCount]

selectClearedTransactionCountByMonth :: Connection -> IO [MonthlyClearedCount]
selectClearedTransactionCountByMonth connection = query_ connection
    "SELECT date_trunc('month', transaction_date)::date, count(*) FROM t_transaction WHERE transaction_state = 'cleared' AND active_status = 'true' GROUP BY date_trunc('month', transaction_date) ORDER BY date_trunc('month', transaction_date)" :: IO [MonthlyClearedCount]

selectDescriptionsUsedMoreThanTenTimes :: Connection -> IO [DescriptionCount]
selectDescriptionsUsedMoreThanTenTimes connection = query_ connection
    "SELECT description, count(*) FROM t_transaction GROUP BY description HAVING count(*) > 10 ORDER BY count(*) DESC" :: IO [DescriptionCount]

selectOrphanedCategories :: Connection -> IO [Category]
selectOrphanedCategories connection = query_ connection
    "SELECT c.category_name,c.category_id,c.owner,c.active_status FROM t_category c LEFT JOIN t_transaction_categories tc ON c.category_id = tc.category_id WHERE tc.transaction_id IS NULL AND NOT EXISTS (SELECT 1 FROM t_transaction t WHERE t.category = c.category_name AND t.owner = c.owner) ORDER BY c.category_name" :: IO [Category]

deleteOrphanedCategory :: Connection -> Integer -> IO ()
deleteOrphanedCategory connection cid = void $
    execute connection
      "DELETE FROM t_category WHERE category_id = ?"
      (Only cid)

selectOrphanedDescriptions :: Connection -> IO [Description]
selectOrphanedDescriptions connection = query_ connection
    "SELECT d.description_name,d.description_id,d.owner,d.active_status FROM t_description d LEFT JOIN t_transaction t ON d.description_name = t.description WHERE t.transaction_id IS NULL ORDER BY d.description_name" :: IO [Description]

deleteOrphanedDescription :: Connection -> Integer -> IO ()
deleteOrphanedDescription connection did = void $
    execute connection
      "DELETE FROM t_description WHERE description_id = ?"
      (Only did)

selectOrphanedReceiptImages :: Connection -> IO [ReceiptImage]
selectOrphanedReceiptImages connection = query_ connection
    "SELECT ri.receipt_image_id,ri.owner,ri.transaction_id,ri.active_status,ri.image_format_type FROM t_receipt_image ri LEFT JOIN t_transaction t ON ri.transaction_id = t.transaction_id WHERE t.transaction_id IS NULL ORDER BY ri.receipt_image_id" :: IO [ReceiptImage]

-- Integrity checks

selectStaleOutstandingTransactions :: Connection -> IO [Transaction]
selectStaleOutstandingTransactions connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE transaction_state IN ('future','outstanding') AND transaction_date < CURRENT_DATE AND active_status = true ORDER BY transaction_date") :: IO [Transaction]

updateTransactionStateToCleared :: Connection -> Integer -> IO ()
updateTransactionStateToCleared connection tid = void $
    execute connection
      "UPDATE t_transaction SET transaction_state = 'cleared' WHERE transaction_id = ?"
      (Only tid)

selectClearedFutureDateTransactions :: Connection -> IO [Transaction]
selectClearedFutureDateTransactions connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE transaction_state = 'cleared' AND transaction_date > CURRENT_DATE AND active_status = true ORDER BY transaction_date") :: IO [Transaction]

selectActiveTransactionsOnInactiveAccounts :: Connection -> IO [Transaction]
selectActiveTransactionsOnInactiveAccounts connection = query_ connection
    ("SELECT t.guid,t.description,t.category,t.account_type,t.account_name_owner,t.notes,t.transaction_state,t.account_id,t.transaction_id,t.reoccurring_type,t.active_status,t.transaction_date,t.amount,t.owner,t.transaction_type FROM t_transaction t JOIN t_account a ON t.account_name_owner = a.account_name_owner WHERE a.active_status = false AND t.active_status = true ORDER BY t.account_name_owner,t.transaction_date") :: IO [Transaction]

selectUncategorizedTransactions :: Connection -> IO [Transaction]
selectUncategorizedTransactions connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE (category = '' OR category = 'undefined') AND active_status = true ORDER BY transaction_date DESC") :: IO [Transaction]

selectUndefinedTransactionType :: Connection -> IO [Transaction]
selectUndefinedTransactionType connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE transaction_type = 'undefined' AND active_status = true ORDER BY transaction_date DESC") :: IO [Transaction]

selectZeroAmountTransactions :: Connection -> IO [Transaction]
selectZeroAmountTransactions connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE amount = 0.00 AND active_status = true AND description NOT IN ('account open','account opened','account close','account closed','initial balance','opening balance') ORDER BY transaction_date DESC") :: IO [Transaction]

selectZeroAmountInactiveTransactions :: Connection -> IO [Transaction]
selectZeroAmountInactiveTransactions connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE amount = 0.00 AND active_status = false AND description NOT IN ('account open','account opened','account close','account closed','initial balance','opening balance') ORDER BY transaction_date DESC") :: IO [Transaction]

selectZeroAmountInactiveTransactionsOnActiveAccounts :: Connection -> IO [Transaction]
selectZeroAmountInactiveTransactionsOnActiveAccounts connection = query_ connection
    ("SELECT t.guid,t.description,t.category,t.account_type,t.account_name_owner,t.notes,t.transaction_state,t.account_id,t.transaction_id,t.reoccurring_type,t.active_status,t.transaction_date,t.amount,t.owner,t.transaction_type FROM t_transaction t JOIN t_account a ON t.account_name_owner = a.account_name_owner WHERE t.amount = 0.00 AND t.active_status = false AND a.active_status = true AND t.description NOT IN ('account open','account opened','account close','account closed','initial balance','opening balance') ORDER BY t.transaction_date DESC") :: IO [Transaction]

deactivateTransaction :: Connection -> Integer -> IO ()
deactivateTransaction connection tid = void $
    execute connection
      "UPDATE t_transaction SET active_status = false WHERE transaction_id = ?"
      (Only tid)

deleteTransactionCascade :: Connection -> Transaction -> IO ()
deleteTransactionCascade connection t = do
    let tid  = transactionTransactionId t
        guid = transactionGuid t
    void $ execute connection
        "DELETE FROM t_transfer WHERE guid_source = ? OR guid_destination = ?"
        (guid, guid)
    void $ execute connection
        "DELETE FROM t_payment WHERE guid_source = ? OR guid_destination = ?"
        (guid, guid)
    void $ execute connection
        "DELETE FROM t_transaction WHERE transaction_id = ?"
        (Only tid)

deleteTransaction :: Connection -> Integer -> IO ()
deleteTransaction connection tid = void $
    execute connection
      "DELETE FROM t_transaction WHERE transaction_id = ?"
      (Only tid)

selectDanglingPayments :: Connection -> IO [Payment]
selectDanglingPayments connection = query_ connection
    "SELECT payment_id,source_account,destination_account,transaction_date,amount,guid_source,guid_destination FROM t_payment WHERE guid_source NOT IN (SELECT guid FROM t_transaction) OR guid_destination NOT IN (SELECT guid FROM t_transaction) ORDER BY payment_id" :: IO [Payment]

selectDanglingTransfers :: Connection -> IO [Transfer]
selectDanglingTransfers connection = query_ connection
    "SELECT transfer_id,source_account,destination_account,transaction_date,amount,guid_source,guid_destination FROM t_transfer WHERE guid_source NOT IN (SELECT guid FROM t_transaction) OR guid_destination NOT IN (SELECT guid FROM t_transaction) ORDER BY transfer_id" :: IO [Transfer]

selectTransactionsWithMismatchedAccountType :: Connection -> IO [Transaction]
selectTransactionsWithMismatchedAccountType connection = query_ connection
    ("SELECT t.guid,t.description,t.category,t.account_type,t.account_name_owner,t.notes,t.transaction_state,t.account_id,t.transaction_id,t.reoccurring_type,t.active_status,t.transaction_date,t.amount,t.owner,t.transaction_type FROM t_transaction t JOIN t_account a ON t.account_name_owner = a.account_name_owner WHERE t.account_type != a.account_type AND t.active_status = true ORDER BY t.account_name_owner") :: IO [Transaction]

selectAccountsNeverValidated :: Connection -> IO [Account]
selectAccountsNeverValidated connection = query_ connection
    "SELECT account_name_owner,account_id,account_type,active_status,moniker,owner,outstanding,future,cleared FROM t_account WHERE validation_date = TO_TIMESTAMP(0) AND active_status = true ORDER BY account_name_owner" :: IO [Account]

selectAccountsWithNoTransactions :: Connection -> IO [Account]
selectAccountsWithNoTransactions connection = query_ connection
    "SELECT a.account_name_owner,a.account_id,a.account_type,a.active_status,a.moniker,a.owner,a.outstanding,a.future,a.cleared FROM t_account a LEFT JOIN t_transaction t ON a.account_name_owner = t.account_name_owner WHERE t.transaction_id IS NULL AND a.active_status = true ORDER BY a.account_name_owner" :: IO [Account]

selectDescriptionsWithWhitespace :: Connection -> IO [Description]
selectDescriptionsWithWhitespace connection = query_ connection
    "SELECT description_name,description_id,owner,active_status FROM t_description WHERE description_name != trim(description_name) ORDER BY description_name" :: IO [Description]

selectPendingTransactionsOlderThan30Days :: Connection -> IO [PendingTransaction]
selectPendingTransactionsOlderThan30Days connection = query_ connection
    "SELECT pending_transaction_id,account_name_owner,transaction_date,description,amount,review_status,owner,date_added FROM t_pending_transaction WHERE review_status = 'pending' AND date_added < now() - interval '30 days' ORDER BY date_added" :: IO [PendingTransaction]

-- New integrity checks (batch 2)

data AccountTotalsDiscrepancy = AccountTotalsDiscrepancy
    { atdAccountNameOwner    :: String
    , atdStoredCleared       :: Scientific
    , atdComputedCleared     :: Scientific
    , atdStoredOutstanding   :: Scientific
    , atdComputedOutstanding :: Scientific
    , atdStoredFuture        :: Scientific
    , atdComputedFuture      :: Scientific
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow AccountTotalsDiscrepancy
instance ToRow AccountTotalsDiscrepancy

selectAccountTotalsOutOfSync :: Connection -> IO [AccountTotalsDiscrepancy]
selectAccountTotalsOutOfSync connection = query_ connection
    "SELECT a.account_name_owner, a.cleared, COALESCE(clr.total,0.00), a.outstanding, COALESCE(ost.total,0.00), a.future, COALESCE(fut.total,0.00) FROM t_account a LEFT JOIN (SELECT account_name_owner, SUM(amount) AS total FROM t_transaction WHERE transaction_state='cleared' AND active_status=true GROUP BY account_name_owner) clr ON a.account_name_owner=clr.account_name_owner LEFT JOIN (SELECT account_name_owner, SUM(amount) AS total FROM t_transaction WHERE transaction_state='outstanding' AND active_status=true GROUP BY account_name_owner) ost ON a.account_name_owner=ost.account_name_owner LEFT JOIN (SELECT account_name_owner, SUM(amount) AS total FROM t_transaction WHERE transaction_state='future' AND active_status=true GROUP BY account_name_owner) fut ON a.account_name_owner=fut.account_name_owner WHERE a.active_status=true AND (a.cleared!=COALESCE(clr.total,0.00) OR a.outstanding!=COALESCE(ost.total,0.00) OR a.future!=COALESCE(fut.total,0.00)) ORDER BY a.account_name_owner" :: IO [AccountTotalsDiscrepancy]

data AccountValidationMismatch = AccountValidationMismatch
    { avmAccountNameOwner     :: String
    , avmStoredValidationDate :: LocalTime
    , avmActualValidationDate :: LocalTime
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow AccountValidationMismatch
instance ToRow AccountValidationMismatch

selectAccountValidationDateOutOfSync :: Connection -> IO [AccountValidationMismatch]
selectAccountValidationDateOutOfSync connection = query_ connection
    "SELECT a.account_name_owner, a.validation_date, MAX(va.validation_date) FROM t_account a JOIN t_validation_amount va ON a.account_id=va.account_id WHERE a.active_status=true AND va.active_status=true GROUP BY a.account_name_owner, a.validation_date HAVING a.validation_date != MAX(va.validation_date) ORDER BY a.account_name_owner" :: IO [AccountValidationMismatch]

selectInactiveDescriptionsStillUsed :: Connection -> IO [Description]
selectInactiveDescriptionsStillUsed connection = query_ connection
    "SELECT DISTINCT d.description_name,d.description_id,d.owner,d.active_status FROM t_description d JOIN t_transaction t ON d.description_name=t.description WHERE d.active_status=false AND t.active_status=true ORDER BY d.description_name" :: IO [Description]

selectInactiveCategoriesStillUsed :: Connection -> IO [Category]
selectInactiveCategoriesStillUsed connection = query_ connection
    "SELECT DISTINCT c.category_name,c.category_id,c.owner,c.active_status FROM t_category c JOIN t_transaction t ON c.category_name=t.category WHERE c.active_status=false AND t.active_status=true ORDER BY c.category_name" :: IO [Category]

selectTransactionsWithNegativeAmount :: Connection -> IO [Transaction]
selectTransactionsWithNegativeAmount connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE amount < 0.00 AND active_status=true ORDER BY amount") :: IO [Transaction]

selectTransactionsWithUnreasonableDate :: Connection -> IO [Transaction]
selectTransactionsWithUnreasonableDate connection = query_ connection
    ("SELECT " <> txCols <> " FROM t_transaction WHERE (transaction_date < '1990-01-01' OR transaction_date > CURRENT_DATE + interval '2 years') AND active_status=true ORDER BY transaction_date") :: IO [Transaction]

data TransactionCategory = TransactionCategory
    { tcCategoryId    :: Integer
    , tcTransactionId :: Integer
    , tcOwner         :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow TransactionCategory
instance ToRow TransactionCategory

selectOrphanedTransactionCategories :: Connection -> IO [TransactionCategory]
selectOrphanedTransactionCategories connection = query_ connection
    "SELECT tc.category_id, tc.transaction_id, tc.owner FROM t_transaction_categories tc WHERE tc.transaction_id NOT IN (SELECT transaction_id FROM t_transaction) OR tc.category_id NOT IN (SELECT category_id FROM t_category) ORDER BY tc.transaction_id" :: IO [TransactionCategory]

selectSelfReferencingPayments :: Connection -> IO [Payment]
selectSelfReferencingPayments connection = query_ connection
    "SELECT payment_id,source_account,destination_account,transaction_date,amount,guid_source,guid_destination FROM t_payment WHERE source_account=destination_account ORDER BY payment_id" :: IO [Payment]

selectSelfReferencingTransfers :: Connection -> IO [Transfer]
selectSelfReferencingTransfers connection = query_ connection
    "SELECT transfer_id,source_account,destination_account,transaction_date,amount,guid_source,guid_destination FROM t_transfer WHERE source_account=destination_account ORDER BY transfer_id" :: IO [Transfer]

data MedicalExpense = MedicalExpense
    { medicalExpenseId                   :: Integer
    , medicalExpenseServiceDate          :: Day
    , medicalExpenseBilledAmount         :: Scientific
    , medicalExpensePatientResponsibility :: Scientific
    , medicalExpensePaidAmount           :: Scientific
    , medicalExpenseClaimStatus          :: String
    , medicalExpenseOwner                :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow MedicalExpense
instance ToRow MedicalExpense

selectUnpaidMedicalExpenses :: Connection -> IO [MedicalExpense]
selectUnpaidMedicalExpenses connection = query_ connection
    "SELECT medical_expense_id,service_date,billed_amount,patient_responsibility,paid_amount,claim_status,owner FROM t_medical_expense WHERE transaction_id IS NULL AND active_status=true ORDER BY service_date" :: IO [MedicalExpense]

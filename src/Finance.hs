{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}


module Finance (lookupEnv, sumOfTransactions, extractCategories,
                outstandingTransactions, futureTransactions, sortAndGroupByList, transactionDebits,
                selectAllTransactions, selectAllAccounts, selectAllCategories, selectTransactionsWithDoubleSpaces,
                selectClearedTransactionCountByWeek, selectClearedTransactionCountByMonth,
                selectDescriptionsUsedMoreThanTenTimes, selectOrphanedCategories, selectOrphanedDescriptions,
                selectOrphanedReceiptImages,
                transactionCredits, someUUIDs, isCleared,
                sumOfActiveTransactions, hasTransactionId, findByTransactionId,
                Transaction(..), Account(..), Category(..), Description(..), ReceiptImage(..), WeeklyClearedCount(..),
                MonthlyClearedCount(..), DescriptionCount(..)) where

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
--import qualified Data.UUID.V4 as U4

data Category = Category
    {categoryName :: String,
    categoryId  :: Integer,
    categoryOwner :: String,
    categoryActiveStatus :: Bool
    } deriving (Show, Generic, Eq, ToJSON, FromJSON)

instance FromRow Category
instance ToRow Category

data Account = Account
    {accountAccountNameOwner :: String,
    accountAccountId  :: Integer,
    accountAccountType  :: String,
    accountActiveStatus  :: Bool,
    accountMoniker  :: String,
    accountOwner :: String,
    accountOutstanding :: Scientific,
    accountFuture :: Scientific,
    accountCleared :: Scientific
    } deriving (Show, Eq, Generic, Ord, ToJSON, FromJSON)

instance FromRow Account
instance ToRow Account

data Transaction = Transaction
    { transactionGuid :: String,
      transactionDescription :: String,
      transactionCategory    :: String,
      transactionAccountType :: String,
      transactionAccountNameOwner :: String,
      transactionNotes :: String,
      transactionTransactionState  :: String,
      transactionAccountId  :: Integer,
      transactionTransactionId  :: Integer,
      transactionReoccurringType   :: String,
      transactionActiveStatus  :: Bool,
      transactionTransactionDate  :: Day,
      transactionAmount   :: Scientific,
      transactionOwner :: String,
      transactionTransactionType :: String
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

--guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring_type,active_status,transaction_date,amount,owner,transaction_type

instance FromRow Transaction
instance ToRow Transaction

someUUIDs :: [UUID]
someUUIDs =
  let seed = 137
      g0 = mkStdGen seed -- RNG from seed
      (u1, g1) = random g0
      (u2, g2) = random g1
      (u3, g3) = random g2
      (u4, g4) = random g3
  in [u1,u2,u3,u4]

printOutstandingTransactions :: Transaction -> IO ()
printOutstandingTransactions transaction =
  when (isOutstanding transaction) $ print (transactionDescription transaction)

sumOfTransactions:: [Transaction] -> Scientific
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

findByTransactionId :: Integer -> [Transaction]  -> Maybe Transaction
findByTransactionId id = find (\f -> transactionTransactionId f == id)

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

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

--isReoccurring :: Transaction -> Bool
--isReoccurring = transactionReoccurring

transactionCredits :: [Transaction] -> [Transaction]
transactionCredits = filter isCredit

transactionDebits :: [Transaction] -> [Transaction]
transactionDebits = filter isDebit

--transactionsReoccurring :: [Transaction] -> [Transaction]
--transactionsReoccurring = filter isReoccurring

extractCategories :: [Transaction] -> [String]
extractCategories xs = transactionCategory <$> xs

sortAndGroupByList :: Ord a => [a] -> [(a, Int)]
sortAndGroupByList transactions = map (\g@(x:_) -> (x, length g)) $ group $ sort transactions

selectAllTransactions :: Connection -> IO [Transaction]
selectAllTransactions connection = query_ connection "SELECT guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring_type,active_status,transaction_date,amount,owner,transaction_type FROM t_transaction WHERE active_status='true'" :: IO [Transaction]

selectAllAccounts :: Connection -> IO [Account]
selectAllAccounts connection = query_ connection "SELECT account_name_owner,account_id,account_type,active_status,moniker,owner,outstanding,future,cleared FROM t_account WHERE active_status='true'" :: IO [Account]

selectAllCategories :: Connection -> IO [Category]
selectAllCategories connection = query_ connection "SELECT category_name,category_id,owner,active_status FROM t_category WHERE active_status='true'" :: IO [Category]

selectTransactionsWithDoubleSpaces :: Connection -> IO [Transaction]
selectTransactionsWithDoubleSpaces connection = query_ connection "SELECT guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring_type,active_status,transaction_date,amount,owner,transaction_type FROM t_transaction WHERE description LIKE '%  %' OR notes LIKE '%  %'" :: IO [Transaction]

data WeeklyClearedCount = WeeklyClearedCount
    { weekStart  :: Day,
      clearedCount :: Integer
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow WeeklyClearedCount
instance ToRow WeeklyClearedCount

selectClearedTransactionCountByWeek :: Connection -> IO [WeeklyClearedCount]
selectClearedTransactionCountByWeek connection = query_ connection "SELECT date_trunc('week', transaction_date)::date, count(*) FROM t_transaction WHERE transaction_state = 'cleared' AND active_status = 'true' GROUP BY date_trunc('week', transaction_date) ORDER BY date_trunc('week', transaction_date)" :: IO [WeeklyClearedCount]

data MonthlyClearedCount = MonthlyClearedCount
    { monthStart   :: Day,
      monthlyClearedCount :: Integer
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow MonthlyClearedCount
instance ToRow MonthlyClearedCount

selectClearedTransactionCountByMonth :: Connection -> IO [MonthlyClearedCount]
selectClearedTransactionCountByMonth connection = query_ connection "SELECT date_trunc('month', transaction_date)::date, count(*) FROM t_transaction WHERE transaction_state = 'cleared' AND active_status = 'true' GROUP BY date_trunc('month', transaction_date) ORDER BY date_trunc('month', transaction_date)" :: IO [MonthlyClearedCount]

data DescriptionCount = DescriptionCount
    { descriptionCountName :: String,
      descriptionCount     :: Integer
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow DescriptionCount
instance ToRow DescriptionCount

selectDescriptionsUsedMoreThanTenTimes :: Connection -> IO [DescriptionCount]
selectDescriptionsUsedMoreThanTenTimes connection = query_ connection "SELECT description, count(*) FROM t_transaction GROUP BY description HAVING count(*) > 10 ORDER BY count(*) DESC" :: IO [DescriptionCount]

selectOrphanedCategories :: Connection -> IO [Category]
selectOrphanedCategories connection = query_ connection "SELECT c.category_name,c.category_id,c.owner,c.active_status FROM t_category c LEFT JOIN t_transaction_categories tc ON c.category_id = tc.category_id WHERE tc.transaction_id IS NULL ORDER BY c.category_name" :: IO [Category]

data Description = Description
    { descriptionName   :: String,
      descriptionId     :: Integer,
      descriptionOwner  :: String,
      descriptionActiveStatus :: Bool
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow Description
instance ToRow Description

selectOrphanedDescriptions :: Connection -> IO [Description]
selectOrphanedDescriptions connection = query_ connection "SELECT d.description_name,d.description_id,d.owner,d.active_status FROM t_description d LEFT JOIN t_transaction t ON d.description_name = t.description WHERE t.transaction_id IS NULL ORDER BY d.description_name" :: IO [Description]

data ReceiptImage = ReceiptImage
    { receiptImageId          :: Integer,
      receiptImageOwner       :: String,
      receiptImageTransactionId :: Integer,
      receiptImageActiveStatus :: Bool,
      receiptImageFormatType  :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow ReceiptImage
instance ToRow ReceiptImage

selectOrphanedReceiptImages :: Connection -> IO [ReceiptImage]
selectOrphanedReceiptImages connection = query_ connection "SELECT ri.receipt_image_id,ri.owner,ri.transaction_id,ri.active_status,ri.image_format_type FROM t_receipt_image ri LEFT JOIN t_transaction t ON ri.transaction_id = t.transaction_id WHERE t.transaction_id IS NULL ORDER BY ri.receipt_image_id" :: IO [ReceiptImage]


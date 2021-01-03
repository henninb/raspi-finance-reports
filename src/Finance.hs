{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}


module Finance (lookupEnv, sumOfTransactions, extractCategories,
                outstandingTransactions, futureTransactions, sortAndGroupByList, transactionsReoccurring, transactionDebits,
                selectAllTransactions, selectAllAccounts, transactionCredits, someUUIDs, isCleared,
                sumOfActiveTransactions, hasTransactionId, findByTransactionId,
                Transaction(..), Account(..), Category(..)) where

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
    {category :: String,
    categoryId  :: Integer
    } deriving (Show, Generic, Eq, ToJSON, FromJSON)

instance FromRow Category
instance ToRow Category

data Account = Account
    {accountAccountNameOwner :: String,
    accountAccountId  :: Integer,
    accountAccountType  :: String,
    accountActiveStatus  :: Bool,
    accountMoniker  :: String
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
      transactionReoccurring   :: Bool,
      transactionActiveStatus  :: Bool,
      transactionTransactionDate  :: Day,
      transactionAmount   :: Scientific
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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

isReoccurring :: Transaction -> Bool
isReoccurring = transactionReoccurring

transactionCredits :: [Transaction] -> [Transaction]
transactionCredits = filter isCredit

transactionDebits :: [Transaction] -> [Transaction]
transactionDebits = filter isDebit

transactionsReoccurring :: [Transaction] -> [Transaction]
transactionsReoccurring = filter isReoccurring

extractCategories :: [Transaction] -> [String]
extractCategories xs = transactionCategory <$> xs

sortAndGroupByList :: Ord a => [a] -> [(a, Int)]
sortAndGroupByList transactions = map (head &&& length) $ group $ sort transactions

selectAllTransactions :: Connection -> IO [Transaction]
selectAllTransactions connection = query_ connection "SELECT guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount FROM t_transaction WHERE active_status='true'" :: IO [Transaction]

selectAllAccounts :: Connection -> IO [Account]
selectAllAccounts connection = query_ connection "SELECT account_name_owner,account_id,account_type,active_status,moniker FROM t_account WHERE active_status='true'" :: IO [Account]


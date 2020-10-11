{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Finance (lookupEnv, addTransactions, extractCategories,
                countOutstanding, sortAndGroupByList, transactionsReoccurring, transactionDebits,
                insertTransaction, selectAllTransactions, selectAllAccounts,
                transactionCredits, someUUIDs, Account, Transaction, Transaction (..)) where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Parser
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import System.Posix.Types(EpochTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import qualified Data.Time as Time
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

-- TODO: not being used
import qualified Network.HTTP.Conduit as NHC
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans

import Control.Monad

import qualified Data.Vector as V
-- TODO: not being used
import qualified Network.HTTP.Client as N

import Network.Wreq
--import Control.Lens (^.)
import Data.Aeson.Lens

import qualified Data.Map.Strict as Map
import Data.Map (Map())
import Control.Arrow

--import Web.Spock
--import Web.Spock.Config

-- import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock
import Data.List
import Data.Typeable
import System.Directory
import Data.String.Utils
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import Data.Ratio
import Data.Int
import Data.Number.CReal
import Data.Scientific
import System.Environment
import System.Exit
import System.IO
-- import Text.Parse
import Options.Applicative
import Data.Semigroup ((<>))
import Data.UUID
import System.Random
import Control.Monad.Random.Strict
import qualified Data.UUID.V4 as U4

type AnotherType = String
data MyType = MyType {
   uuid :: UUID,
   elements :: AnotherType
} deriving (Show)

type M = Rand StdGen

-- smart constructor for MyType with unique UUID
--myType :: AnotherType -> M MyType
--myType x = MyType <$> getRandom <*> pure x

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

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

someUUIDs :: [UUID]
someUUIDs =
  let seed = 137
      g0 = mkStdGen seed -- RNG from seed
      (u1, g1) = random g0
      (u2, g2) = random g1
      (u3, g3) = random g2
      (u4, g4) = random g3
  in [u1,u2,u3,u4]

transaction :: Transaction
transaction = Transaction "653fc2a9-14b9-4318-bcb3-178c59458f61" "test" "test" "credit" "chase_kari" "" "cleared" 1013 1 True True (parseDay "2020-12-31") 0.0

printOutstandingTransactions :: Transaction -> IO ()
printOutstandingTransactions transaction =
  when (transactionTransactionState transaction == "cleared") $ print (transactionDescription transaction)

countTransactionsList:: [Transaction] -> Int
countTransactionsList = foldr (\ x -> (+) 1) 0

addTransactions:: [Transaction] -> Scientific
addTransactions = foldr ((+) . transactionAmount) 0.0

countOutstanding :: Num a => [Transaction] -> a
countOutstanding []  = 0
countOutstanding (x:xs) =  if transactionTransactionState x == "cleared" then 1 + countOutstanding xs else 0 + countOutstanding xs

isOutstanding x = transactionTransactionState x == "outstanding"
isCleared x = transactionTransactionState x == "cleared"
isFuture x = transactionTransactionState x == "cleared"
isCredit x = transactionAccountType x == "credit"
isDebit x = transactionAccountType x == "debit"
-- isReoccurring :: Transaction -> Bool
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
sortAndGroupByList myList = map (head &&& length) $ group $ sort myList

insertTransaction :: Connection -> Transaction -> IO Int64
insertTransaction connection = execute connection "INSERT INTO t_transaction (guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"

selectAllTransactions :: Connection -> IO [Transaction]
selectAllTransactions connection = query_ connection "SELECT guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount FROM t_transaction" :: IO [Transaction]

selectAllAccounts :: Connection -> IO [Account]
selectAllAccounts connection = query_ connection "SELECT account_name_owner,account_id,account_type,active_status,moniker FROM t_account" :: IO [Account]

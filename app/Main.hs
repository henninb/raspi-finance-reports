{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Main where

import Lib

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
import GHC.Generics (Generic)
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



data Category = Category
    {category :: String,
    categoryId  :: Integer
    } deriving (Show, Eq)

data Account = Account
    {accountAccountNameOwner :: String,
    accountAccountId  :: Integer,
    accountAccountType  :: String,
    accountActiveStatus  :: Bool,
    accountMoniker  :: String
    } deriving (Show, Eq, Generic, Ord)

instance FromRow Account
instance ToRow Account

data Transaction = Transaction
    { transactionGuid :: String,
      transactionDescription :: String,
      transactionCategory    :: String,
      transactionAccountType :: String,
      transactionAccountNameOwner :: String,
      transactionNotes :: String,
      transactionCleared  :: Integer,
      transactionAccountId  :: Integer,
      transactionTransactionId  :: Integer,
      transactionReoccurring   :: Bool,
      transactionTransactionDate  :: Day,
      transactionAmount   :: Scientific
    } deriving (Show, Eq, Generic, Ord)

instance FromRow Transaction
instance ToRow Transaction

selectAllTransactions :: Connection -> IO [Transaction]
selectAllTransactions connection = query_ connection "SELECT guid,description,category,account_type,account_name_owner,notes,cleared,account_id,transaction_id,reoccurring,transaction_date,amount FROM t_transaction" :: IO [Transaction]

selectAllAccounts :: Connection -> IO [Account]
selectAllAccounts connection = query_ connection "SELECT account_name_owner,account_id,account_type,active_status,moniker FROM t_account" :: IO [Account]

printOutstandingTransactions :: Transaction -> IO ()
printOutstandingTransactions transaction =
  when (transactionCleared transaction == 0) $ print (transactionDescription transaction)

count:: [Transaction] -> Int
count = foldr (\ x -> (+) 1) 0

addTransactions:: [Transaction] -> Scientific
addTransactions = foldr ((+) . transactionAmount) 0.0

countOutstanding :: Num a => [Transaction] -> a
countOutstanding []  = 0
countOutstanding (x:xs) =  if transactionCleared x == 0 then 1 + countOutstanding xs else 0 + countOutstanding xs

isOutstanding x = transactionCleared x == 0
isCleared x = transactionCleared x == 1
isFuture x = transactionCleared x == 1
isCredit x = transactionAccountType x == "credit"
isDebit x = transactionAccountType x == "debit"
isReoccurring x = transactionReoccurring x

transactionCredits :: [Transaction] -> [Transaction]
transactionCredits = filter isCredit

transactionDebits :: [Transaction] -> [Transaction]
transactionDebits = filter isDebit

transactionsReoccurring :: [Transaction] -> [Transaction]
transactionsReoccurring = filter isReoccurring

main :: IO ()
main = do
  putStrLn "--- separated ---"
  programName <- getProgName
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args then
    print "help" >> exitSuccess
  else if "-v" `elem` args || "--version" `elem` args then
    print "version" >> exitSuccess
  else
    print "running main program..."
  putStrLn programName

  putStrLn "--- separated ---"
  postgreqlUsername <- lookupEnv "POSTGRESQL_USESRNAME"
  print postgreqlUsername
  postgreqlPassword <- lookupEnv "POSTGRESQL_PASSWORD"
  print postgreqlPassword
  putStrLn "--- separated ---"
  connection <- connect defaultConnectInfo { connectHost = "localhost", connectDatabase = "finance_db", connectUser = "henninb", connectPassword = "monday1"}
  transactions <- selectAllTransactions connection
  accounts <- selectAllAccounts connection
  let credits = transactionCredits transactions
  let debits = transactionDebits transactions
  let reoccurring = transactionsReoccurring transactions
  print (length transactions)
  print (length accounts)
  print (countOutstanding transactions)
  print (length credits)
  print (length debits)
  print (addTransactions credits)
  print (addTransactions debits)
  print (length reoccurring)
  putStrLn "--- need to search by fuel and restaurant ---"
  putStrLn "--- separated ---"


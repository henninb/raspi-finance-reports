{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Finance (lookupEnv, sumOfTransactions, extractCategories,
                outstandingTransactions, futureTransactions, sortAndGroupByList, transactionsReoccurring, transactionDebits,
                selectAllTransactions, selectAllAccounts, transactionCredits, someUUIDs, isCleared, apiService,
                sumOfActiveTransactions,
                Transaction(..), Account(..)) where

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
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

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


-- TODO: move this code to the Main
-- http://localhost:3000/transaction/

type TransactionApi =
  "transaction" :> Get '[JSON] [Transaction] :<|>
  --TODO: this code does not work
  "transaction" :> Capture "transactionId" Integer :> Get '[JSON] Transaction

transactionApi :: Proxy TransactionApi
transactionApi = Proxy

apiService :: IO ()
apiService = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve transactionApi server

server :: Server TransactionApi
server =
  getTransactions :<|>
  getTransactionById

getTransactions :: Handler [Transaction]
getTransactions = return [exampleTransaction]

getTransactionById :: Integer -> Handler Transaction
getTransactionById = \ case
  0 -> return exampleTransaction
  _ -> throwError err404

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

exampleTransaction :: Transaction
exampleTransaction = Transaction "653fc2a9-14b9-4318-bcb3-178c59458f61" "test" "test" "credit" "chase_kari" "" "cleared" 1001 1002 True True (parseDay "2020-12-31") 0.0

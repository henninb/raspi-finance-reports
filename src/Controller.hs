{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Controller where
import Finance
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
--import Data.Time
import Database.PostgreSQL.Simple
import Text.Printf

type TransactionApi =
  -- localhost:3000/
  Get '[JSON] String
  -- localhost:3000/transaction
  :<|> "transaction" :> Get '[JSON] [Transaction]
  :<|> "first" :> Get '[JSON] Transaction
     -- http://localhost:3000/transaction/1006
  :<|> "transaction" :> Capture "id" Int :> Get '[JSON] Int

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
mkApp = do
    connection <- connect defaultConnectInfo { connectHost = "192.168.100.124", connectDatabase = "finance_db", connectUser = "henninb", connectPassword = "monday1"}
    transactions <- selectAllTransactions connection
    accounts <- selectAllAccounts connection
    let credits = transactionCredits transactions
    let debits = transactionDebits transactions
    let reoccurring = transactionsReoccurring transactions
    let categoriesList = extractCategories transactions
    let categoriesCount = sortAndGroupByList categoriesList

    printf "Transaction Quantity: %d\n" (length transactions)
    printf "Account Quantity: %d\n" (length accounts)
    printf "Transactions Outstanding: %d\n" (length (outstandingTransactions transactions))
    printf "Transactions Future: %d\n" (length (futureTransactions transactions))
    printf "Credits Quantity: %d\n"  (length credits)
    printf "Debits Quantity: %d\n" (length debits)
    print (sumOfActiveTransactions credits)
    print (sumOfActiveTransactions debits)
    printf "Reoccurring Quantity: %d\n" (length reoccurring)
    printf "Category Quantity: %d\n" (length categoriesCount)
    return $ serve transactionApi (server transactions)

server :: [Transaction] -> Server TransactionApi
server transactions =
  getRoot
  :<|> getTransactions transactions
  :<|> getTransactionFirst transactions
  :<|> getTransactionById transactions

getTransactions :: [Transaction] -> Handler [Transaction]
getTransactions = return

-- http://localhost:3000/test/1006
getTransactionById :: [Transaction] -> Int -> Handler Int
getTransactionById transactions = return
--getTransactionById transactions 1002 = return (head transactions)
--getTransactionById transactions 1003 = return (head transactions)
--getTransactionById transactions 1004 = return (head transactions)
--getTransactionById _ _ = throwError err404

getTransactionFirst :: [Transaction] -> Handler Transaction
getTransactionFirst transactions = return (head transactions)

getRoot :: Handler String
getRoot = return "{}"
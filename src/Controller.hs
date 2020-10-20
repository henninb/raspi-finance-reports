{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Controller where
import Finance
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Database.PostgreSQL.Simple
import Text.Printf
import Data.List

type TransactionApi =
  -- localhost:3000/
  Get '[JSON] String
  -- localhost:3000/transaction
  :<|> "transaction" :> Get '[JSON] [Transaction]
  -- http://localhost:3000/transaction/first
  :<|> "transaction" :> "first" :> Get '[JSON] Transaction
  -- http://localhost:3000/transaction/1001
  :<|> "transaction" :> Capture "id" Integer :> Get '[JSON] Transaction

transactionApi :: Proxy TransactionApi
transactionApi = Proxy

apiService :: IO ()
apiService = do
  let port = 3000
  let settings =  setPort port $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
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
    return $ serve transactionApi (server transactions accounts)

server :: [Transaction] -> [Account] -> Server TransactionApi
server transactions accounts =
  getRoot
  :<|> getTransactions transactions
  :<|> getTransactionFirst transactions
  :<|> getTransactionById transactions

getTransactions :: [Transaction] -> Handler [Transaction]
getTransactions = return

-- http://localhost:3000/transaction/1001
getTransactionById :: [Transaction] -> Integer -> Handler Transaction
getTransactionById transactions x = return (fromJust (findByTransactionId x transactions))
--getTransactionById _ _ = throwError err404

getTransactionFirst :: [Transaction] -> Handler Transaction
getTransactionFirst transactions = return (head transactions)

getRoot :: Handler String
getRoot = return "{}"

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

--stringHandler = liftIO ioMaybeString >>= f
--    where f (Just str) = return str
--          f Nothing = throwError err404
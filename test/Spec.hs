{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Finance
import Data.Time
import Data.Int
import qualified Data.ByteString.Lazy as LB
import Database.PostgreSQL.Simple
import Data.Aeson
-- import Data.Data (typeOf)
import Data.ByteString.Internal


parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

insertAccount :: Connection -> Account -> IO Int64
insertAccount connection = execute connection "INSERT INTO t_account(account_name_owner, account_id, account_type, active_status,  moniker, date_added, date_updated) VALUES(?, ?, ?, ?, ?, now(), now())"

insertTransaction :: Connection -> Transaction -> IO Int64
insertTransaction connection = execute connection "INSERT INTO t_transaction (guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"

transaction :: Transaction
transaction = Transaction "653fc2a9-14b9-4318-bcb3-178c59458f61" "test" "test" "credit" "chase_kari" "" "cleared" 1001 1002 True True (parseDay "2020-12-31") 0.0

account :: Account
account = Account "chase_kari" 1001 "credit"  True "0000"

connStr :: ByteString
connStr = "host=localhost dbname=finance_test_db user=henninb password=monday1 port=5432"

loadTestData :: IO ()
loadTestData = do
  connection <- connectPostgreSQL connStr
  _ <- execute_ connection "TRUNCATE TABLE t_transaction RESTART IDENTITY CASCADE"
  _ <- execute_ connection "TRUNCATE TABLE t_account RESTART IDENTITY CASCADE"
  payloadTransactions <- LB.readFile "test-transactions.json"
  let eitherTransactions = eitherDecode payloadTransactions :: Either String [Transaction]

  payloadAccounts <- LB.readFile "test-accounts.json"
  let eitherAccounts = eitherDecode payloadAccounts :: Either String [Account]

  let Right unwrappedTransactions = eitherTransactions
  let Right unwrappedAccounts = eitherAccounts

  mapM_ (insertAccount connection) unwrappedAccounts
  mapM_ (insertTransaction connection) unwrappedTransactions
  _ <- close connection
  putStrLn "--- separated ---"

spec :: Spec
spec = do
    describe "test loaded data" $ do
      it "count the number of transactions" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        _ <- close connection
        length transactions `shouldBe` 6

      it "count the number of accounts" $ do
        connection <- connectPostgreSQL connStr
        accounts <- selectAllAccounts connection
        _ <- close connection
        length accounts `shouldBe` 4

      it "amount of credits" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let credits = transactionCredits transactions
        _ <- close connection
        sumOfActiveTransactions credits `shouldBe` 115.02

      it "amount of credits" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let debits = transactionDebits transactions
        _ <- close connection
        sumOfActiveTransactions debits `shouldBe` 900.05

      it "amount of totals" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let debits = transactionDebits transactions
        let credits = transactionCredits transactions
        _ <- close connection
        sumOfActiveTransactions debits - sumOfActiveTransactions credits `shouldBe` 785.03

      it "amount of credits - cleared" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let debits = transactionDebits transactions
        let debitsCleared = filter isCleared debits
        _ <- close connection
        sumOfActiveTransactions debitsCleared `shouldBe` 900.05

      it "count of reoccurring transactions" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let reoccurring = transactionsReoccurring transactions
        _ <- close connection
        length reoccurring `shouldBe` 1

      it "count of outstanding transactions" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let outstanding = outstandingTransactions transactions
        _ <- close connection
        length outstanding `shouldBe` 1

      it "count of future transactions" $ do
        connection <- connectPostgreSQL connStr
        transactions <- selectAllTransactions connection
        let future = futureTransactions transactions
        _ <- close connection
        length future `shouldBe` 0

main :: IO ()
main = do
  loadTestData
  hspec spec
  putStrLn "--- separated ---"

{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Finance
import Data.Time
import Data.Int
import Data.Time.Calendar
import qualified Data.ByteString.Lazy as LB
import Database.PostgreSQL.Simple
import Data.Aeson (decode, eitherDecode)
import Data.Data (typeOf)
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
--
spec :: Spec
spec = do
    -- connection <- connectPostgreSQL connStr
    -- transactions <- selectAllTransactions connection
    -- accounts <- selectAllAccounts connection
    describe "Describe group of tests" $ do
      it "count the number of transactions" $
        (4) `shouldBe` 4

main :: IO ()
main = do
  connection <- connectPostgreSQL connStr
  _ <- execute_ connection "TRUNCATE TABLE t_transaction RESTART IDENTITY CASCADE"
  _ <- execute_ connection "TRUNCATE TABLE t_account RESTART IDENTITY CASCADE"
  payloadTransactions <- LB.readFile "test-transactions.json"
  let eitherTransactions = eitherDecode payloadTransactions :: Either String [Transaction]

  payloadAccounts <- LB.readFile "test-accounts.json"
  let eitherAccounts = eitherDecode payloadAccounts :: Either String [Account]

  let Right unwrappedTransactions = eitherTransactions
  let Right unwrappedAccounts = eitherAccounts

  _ <- mapM (insertAccount connection) unwrappedAccounts
  _ <- mapM (insertTransaction connection) unwrappedTransactions
--
  transactions <- selectAllTransactions connection
  accounts <- selectAllAccounts connection
  let credits = transactionCredits transactions
  let debits = transactionDebits transactions
  let reoccurring = transactionsReoccurring transactions
  hspec spec
  print (length accounts)
  print (length transactions)
  print (addTransactions credits)
  print (addTransactions debits)

  putStrLn "--- separated ---"

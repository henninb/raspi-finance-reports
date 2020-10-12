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
-- import           Database.Persist.Postgresql

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

insertAccount :: Connection -> Account -> IO Int64
-- insertAccount connection = execute connection "INSERT INTO t_account(account_name_owner, account_id, account_type, active_status,  moniker) VALUES('chase_kari', 1001, 'credit', '0000')"
insertAccount connection = execute connection "INSERT INTO t_account(account_name_owner, account_id, account_type, active_status,  moniker) VALUES(?, ?, ?, ?, ?)"

insertTransaction :: Connection -> Transaction -> IO Int64
insertTransaction connection = execute connection "INSERT INTO t_transaction (guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"

transaction :: Transaction
transaction = Transaction "653fc2a9-14b9-4318-bcb3-178c59458f61" "test" "test" "credit" "chase_kari" "" "cleared" 1001 1002 True True (parseDay "2020-12-31") 0.0

transaction1 :: Transaction
transaction1 = Transaction "653fc2a9-14b9-4318-bcb3-178c59458f62" "test1" "test1" "credit" "chase_kari" "" "cleared" 1001 1001 True True (parseDay "2020-12-31") 1.0

account :: Account
account = Account "chase_kari" 1001 "credit"  True "0000"
-- connStr :: String
connStr = "host=localhost dbname=finance_test_db user=henninb password=monday1 port=5432"

-- clearTables = void $ withConnection $ \conn ->
--   mapM_ @[] (\table -> execute_ conn $ "TRUNCATE TABLE " <> table <> " RESTART IDENTITY CASCADE")

-- x = do
--   conn <- connectTest
--   _ <- run conn "TRUNCATE TABLE scores;" []
--   commit conn
--   disconnect conn
-- spec1 :: Spec
-- spec1 = describeDB init "Queue" $ do
--   itDB "empty gives nothing" $ do
--     tryLockDB
--       `shouldReturn` Nothing
spec :: Spec
spec =
    describe "Describe group of tests" $ do
      it "describe the test" $
        0 `shouldBe` 0

main :: IO ()
main = do
  conn <- connectPostgreSQL connStr
  _ <- execute_ conn "TRUNCATE TABLE t_transaction RESTART IDENTITY CASCADE"
  _ <- execute_ conn "TRUNCATE TABLE t_account RESTART IDENTITY CASCADE"

  payloadTransactions <- LB.readFile "test-transactions.json"
  let eitherTransactions = eitherDecode payloadTransactions :: Either String [Transaction]
  let Right transactions = eitherTransactions

  payloadAccounts <- LB.readFile "test-accounts.json"
  let eitherAccounts = eitherDecode payloadAccounts :: Either String [Account]
  print (typeOf eitherAccounts)
  let Right accounts = eitherAccounts
  let _ = map (insertAccount conn) accounts
  let _ = map (insertTransaction conn) transactions

  print transactions
  --insertAccount conn account
  --print (typeOf insertAccounts)
  --insertTransaction conn transaction
  --insertTransaction conn transaction1
  hspec spec

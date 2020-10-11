{-# LANGUAGE OverloadedStrings #-}
module Main where

import Finance
import Database.PostgreSQL.Simple
import System.Exit
import System.Environment
import Data.Int

-- insertTransaction :: Connection -> Transaction -> IO Int64
-- insertTransaction connection = execute connection "INSERT INTO t_transaction (guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"

-- selectAllTransactions :: Connection -> IO [Transaction]
-- selectAllTransactions connection = query_ connection "SELECT guid,description,category,account_type,account_name_owner,notes,transaction_state,account_id,transaction_id,reoccurring,active_status,transaction_date,amount FROM t_transaction" :: IO [Transaction]

-- selectAllAccounts :: Connection -> IO [Account]
-- selectAllAccounts connection = query_ connection "SELECT account_name_owner,account_id,account_type,active_status,moniker FROM t_account" :: IO [Account]

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
  putStrLn "--- separated ---"
  postgresqlUsername <- lookupEnv "POSTGRESQL_USESRNAME"
  postgresqlPassword <- lookupEnv "POSTGRESQL_PASSWORD"
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
  let categoriesList = extractCategories transactions
  let categoriesCount = sortAndGroupByList categoriesList
  print categoriesCount
  print someUUIDs
 -- x <- insertTransaction connection transaction
 -- print x
--  print (sizeOfTransactionMap categoriesMap)
  putStrLn "--- need to search by fuel and restaurant ---"
  putStrLn "--- separated ---"


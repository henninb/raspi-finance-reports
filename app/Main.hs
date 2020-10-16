{-# LANGUAGE OverloadedStrings #-}
module Main where

import Finance
import Database.PostgreSQL.Simple
import System.Exit
import System.Environment
import Data.Int
import Text.Printf

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
  print (sumOfActiveTransactions debits - sumOfActiveTransactions credits)
  printf "Reoccurring Quantity: %d\n" (length reoccurring)
  printf "Category Quantity: %d\n" (length categoriesCount)

  putStrLn "--- TODO: search by fuel and restaurant ---"
  putStrLn "--- separated ---"
  apiService

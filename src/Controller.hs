{-# LANGUAGE OverloadedStrings #-}

module Controller (runMenu) where

import Finance
import Database.PostgreSQL.Simple
import System.IO (hFlush, stdout)
import Text.Printf

type MenuAction = Connection -> IO ()

menuItems :: [(String, MenuAction)]
menuItems =
  [ ("Transactions with double spaces",       reportDoubleSpaces)
  , ("Orphaned descriptions",                 reportOrphanedDescriptions)
  , ("Orphaned categories",                   reportOrphanedCategories)
  , ("Orphaned receipt images",               reportOrphanedReceiptImages)
  , ("Descriptions used more than 10 times",  reportFrequentDescriptions)
  , ("Cleared transaction count by week",     reportWeeklyCleared)
  , ("Cleared transaction count by month",    reportMonthlyCleared)
  ]

printMenu :: IO ()
printMenu = do
  putStrLn "\n=== Finance Data Quality Reports ==="
  mapM_ (\(i, (label, _)) -> printf "  %d. %s\n" (i :: Int) label) (zip [1..] menuItems)
  putStrLn "  0. Exit"
  putStr "\nSelect an option: "
  hFlush stdout

runMenu :: Connection -> IO ()
runMenu conn = do
  printMenu
  line <- getLine
  case reads line :: [(Int, String)] of
    [(0, "")] -> putStrLn "Goodbye."
    [(n, "")] | n >= 1 && n <= length menuItems -> do
      let (_, action) = menuItems !! (n - 1)
      action conn
      runMenu conn
    _ -> do
      putStrLn "Invalid option, try again."
      runMenu conn

reportDoubleSpaces :: MenuAction
reportDoubleSpaces conn = do
  results <- selectTransactionsWithDoubleSpaces conn
  printf "\n--- Transactions with double spaces (%d) ---\n" (length results)
  mapM_ printTransaction results

reportOrphanedDescriptions :: MenuAction
reportOrphanedDescriptions conn = do
  results <- selectOrphanedDescriptions conn
  printf "\n--- Orphaned descriptions (%d) ---\n" (length results)
  mapM_ (\d -> printf "  [%d] %s  (owner: %s)\n"
    (descriptionId d) (descriptionName d) (descriptionOwner d)) results

reportOrphanedCategories :: MenuAction
reportOrphanedCategories conn = do
  results <- selectOrphanedCategories conn
  printf "\n--- Orphaned categories (%d) ---\n" (length results)
  mapM_ (\c -> printf "  [%d] %s  (owner: %s)\n"
    (categoryId c) (categoryName c) (categoryOwner c)) results

reportOrphanedReceiptImages :: MenuAction
reportOrphanedReceiptImages conn = do
  results <- selectOrphanedReceiptImages conn
  printf "\n--- Orphaned receipt images (%d) ---\n" (length results)
  mapM_ (\r -> printf "  [%d] transaction_id: %d  owner: %s  format: %s\n"
    (receiptImageId r) (receiptImageTransactionId r)
    (receiptImageOwner r) (receiptImageFormatType r)) results

reportFrequentDescriptions :: MenuAction
reportFrequentDescriptions conn = do
  results <- selectDescriptionsUsedMoreThanTenTimes conn
  printf "\n--- Descriptions used more than 10 times (%d) ---\n" (length results)
  mapM_ (\d -> printf "  %4d  %s\n" (descriptionCount d) (descriptionCountName d)) results

reportWeeklyCleared :: MenuAction
reportWeeklyCleared conn = do
  results <- selectClearedTransactionCountByWeek conn
  printf "\n--- Cleared transaction count by week (%d weeks) ---\n" (length results)
  mapM_ (\w -> printf "  %s  %d\n" (show (weekStart w)) (clearedCount w)) results

reportMonthlyCleared :: MenuAction
reportMonthlyCleared conn = do
  results <- selectClearedTransactionCountByMonth conn
  printf "\n--- Cleared transaction count by month (%d months) ---\n" (length results)
  mapM_ (\m -> printf "  %s  %d\n" (show (monthStart m)) (monthlyClearedCount m)) results

printTransaction :: Transaction -> IO ()
printTransaction t =
  printf "  [%d] %-40s  %-30s  %-12s  %s\n"
    (transactionTransactionId t)
    (transactionDescription t)
    (transactionAccountNameOwner t)
    (transactionTransactionState t)
    (show (transactionAmount t))

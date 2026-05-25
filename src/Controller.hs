{-# LANGUAGE OverloadedStrings #-}

module Controller (runMenu) where

import Finance
import Database.PostgreSQL.Simple
import System.IO (hFlush, hGetBuffering, hSetBuffering, hGetChar, stdout, stdin, BufferMode(..))
import Control.Monad (when)
import Text.Printf

type MenuAction = Connection -> IO ()
type CountQuery = Connection -> IO Int

count :: IO [a] -> IO Int
count q = length <$> q

menuItems :: [(String, MenuAction, CountQuery)]
menuItems =
  [ ("Transactions with double spaces",                          reportDoubleSpaces,                        \c -> count $ selectTransactionsWithDoubleSpaces c)
  , ("Orphaned descriptions",                                    reportOrphanedDescriptions,                \c -> count $ selectOrphanedDescriptions c)
  , ("Orphaned categories",                                      reportOrphanedCategories,                  \c -> count $ selectOrphanedCategories c)
  , ("Orphaned receipt images",                                  reportOrphanedReceiptImages,               \c -> count $ selectOrphanedReceiptImages c)
  , ("Descriptions used more than 10 times",                     reportFrequentDescriptions,                \c -> count $ selectDescriptionsUsedMoreThanTenTimes c)
  , ("Cleared transaction count by week",                        reportWeeklyCleared,                       \c -> count $ selectClearedTransactionCountByWeek c)
  , ("Cleared transaction count by month",                       reportMonthlyCleared,                      \c -> count $ selectClearedTransactionCountByMonth c)
  , ("Stale outstanding/future transactions",                    reportStaleTransactions,                   \c -> count $ selectStaleOutstandingTransactions c)
  , ("Cleared transactions with future date",                    reportClearedFutureDate,                   \c -> count $ selectClearedFutureDateTransactions c)
  , ("Active transactions on inactive accounts",                 reportActiveOnInactiveAccounts,            \c -> count $ selectActiveTransactionsOnInactiveAccounts c)
  , ("Uncategorized transactions",                               reportUncategorized,                       \c -> count $ selectUncategorizedTransactions c)
  , ("Transactions with undefined type",                         reportUndefinedType,                       \c -> count $ selectUndefinedTransactionType c)
  , ("Zero-amount transactions",                                 reportZeroAmount,                          \c -> count $ selectZeroAmountTransactions c)
  , ("Zero-amount inactive transactions",                        reportZeroAmountInactive,                  \c -> count $ selectZeroAmountInactiveTransactions c)
  , ("Zero-amount inactive transactions on active accounts",     reportZeroAmountInactiveOnActiveAccounts,  \c -> count $ selectZeroAmountInactiveTransactionsOnActiveAccounts c)
  , ("Dangling payments",                                        reportDanglingPayments,                    \c -> count $ selectDanglingPayments c)
  , ("Dangling transfers",                                       reportDanglingTransfers,                   \c -> count $ selectDanglingTransfers c)
  , ("Transactions with mismatched account type",                reportMismatchedAccountType,               \c -> count $ selectTransactionsWithMismatchedAccountType c)
  , ("Accounts never validated",                                 reportNeverValidatedAccounts,              \c -> count $ selectAccountsNeverValidated c)
  , ("Active accounts with no transactions",                     reportAccountsNoTransactions,              \c -> count $ selectAccountsWithNoTransactions c)
  , ("Descriptions with leading/trailing spaces",                reportDescriptionsWhitespace,              \c -> count $ selectDescriptionsWithWhitespace c)
  , ("Pending transactions older than 30 days",                  reportOldPendingTransactions,              \c -> count $ selectPendingTransactionsOlderThan30Days c)
  , ("Account totals out of sync",                               reportAccountTotalsOutOfSync,              \c -> count $ selectAccountTotalsOutOfSync c)
  , ("Account validation date out of sync",                      reportAccountValidationOutOfSync,          \c -> count $ selectAccountValidationDateOutOfSync c)
  , ("Inactive descriptions still used",                         reportInactiveDescriptionsUsed,            \c -> count $ selectInactiveDescriptionsStillUsed c)
  , ("Inactive categories still used",                           reportInactiveCategoriesUsed,              \c -> count $ selectInactiveCategoriesStillUsed c)
  , ("Transactions with negative amount",                        reportNegativeAmounts,                     \c -> count $ selectTransactionsWithNegativeAmount c)
  , ("Transactions with unreasonable date",                      reportUnreasonableDate,                    \c -> count $ selectTransactionsWithUnreasonableDate c)
  , ("Orphaned transaction-category links",                      reportOrphanedTransactionCategories,       \c -> count $ selectOrphanedTransactionCategories c)
  , ("Self-referencing payments",                                reportSelfReferencingPayments,             \c -> count $ selectSelfReferencingPayments c)
  , ("Self-referencing transfers",                               reportSelfReferencingTransfers,            \c -> count $ selectSelfReferencingTransfers c)
  , ("Unpaid medical expenses",                                  reportUnpaidMedicalExpenses,               \c -> count $ selectUnpaidMedicalExpenses c)
  ]

printMenu :: Connection -> IO ()
printMenu conn = do
  putStrLn "\n=== Finance Data Quality Reports ==="
  mapM_ (\(i, (label, _, countQ)) -> do
    n <- countQ conn
    printf "  %2d. %-55s (%d)\n" (i :: Int) label n
    ) (zip [1..] menuItems)
  putStrLn "   0. Exit"
  putStr "\nSelect an option: "
  hFlush stdout

runMenu :: Connection -> IO ()
runMenu conn = do
  printMenu conn
  line <- getLine
  case reads line :: [(Int, String)] of
    [(0, "")] -> putStrLn "Goodbye."
    [(n, "")] | n >= 1 && n <= length menuItems -> do
      let (_, action, _) = menuItems !! (n - 1)
      action conn
      runMenu conn
    _ -> do
      putStrLn "Invalid option, try again."
      runMenu conn

-- Helpers

promptYN :: IO Bool
promptYN = do
  old <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- hGetChar stdin
  hSetBuffering stdin old
  putStrLn ""
  return (c == 'y' || c == 'Y')

printTransaction :: Transaction -> IO ()
printTransaction t =
  printf "  [%d] %-40s  %-30s  %-12s  %s\n"
    (transactionTransactionId t)
    (transactionDescription t)
    (transactionAccountNameOwner t)
    (transactionTransactionState t)
    (show (transactionAmount t))

printAccount :: Account -> IO ()
printAccount a =
  printf "  %-35s  %-10s  outstanding: %s\n"
    (accountAccountNameOwner a)
    (accountAccountType a)
    (show (accountOutstanding a))

printPayment :: Payment -> IO ()
printPayment p =
  printf "  [%d] %s  src: %-25s  dst: %-25s  amount: %s\n"
    (paymentId p)
    (show (paymentTransactionDate p))
    (paymentSourceAccount p)
    (paymentDestinationAccount p)
    (show (paymentAmount p))

printTransfer :: Transfer -> IO ()
printTransfer t =
  printf "  [%d] %s  src: %-25s  dst: %-25s  amount: %s\n"
    (transferId t)
    (show (transferTransactionDate t))
    (transferSourceAccount t)
    (transferDestinationAccount t)
    (show (transferAmount t))

printPendingTransaction :: PendingTransaction -> IO ()
printPendingTransaction p =
  printf "  [%d] %s  %-30s  %-35s  %s  %s\n"
    (pendingTransactionId p)
    (show (pendingTransactionDate p))
    (pendingTransactionAccountNameOwner p)
    (pendingTransactionDescription p)
    (show (pendingTransactionAmount p))
    (pendingTransactionReviewStatus p)

-- Existing reports

reportDoubleSpaces :: MenuAction
reportDoubleSpaces conn = do
  results <- selectTransactionsWithDoubleSpaces conn
  printf "\n--- Transactions with double spaces (%d) ---\n" (length results)
  mapM_ (promptAndFixDoubleSpace conn) results

promptAndFixDoubleSpace :: Connection -> Transaction -> IO ()
promptAndFixDoubleSpace conn t = do
  printTransaction t
  putStr "  Fix? [y/N]: "
  hFlush stdout
  yes <- promptYN
  when yes $ do
    updateTransactionDescriptionFixDoubleSpaces conn t
    putStrLn "  Fixed."

reportOrphanedDescriptions :: MenuAction
reportOrphanedDescriptions conn = do
  results <- selectOrphanedDescriptions conn
  printf "\n--- Orphaned descriptions (%d) ---\n" (length results)
  mapM_ (promptAndFixOrphanedDescription conn) results

promptAndFixOrphanedDescription :: Connection -> Description -> IO ()
promptAndFixOrphanedDescription conn d = do
  printf "  [%d] %s  (owner: %s)\n" (descriptionId d) (descriptionName d) (descriptionOwner d)
  putStr "  Delete? [y/N]: "
  hFlush stdout
  yes <- promptYN
  when yes $ do
    deleteOrphanedDescription conn (descriptionId d)
    putStrLn "  Deleted."

reportOrphanedCategories :: MenuAction
reportOrphanedCategories conn = do
  results <- selectOrphanedCategories conn
  printf "\n--- Orphaned categories (%d) ---\n" (length results)
  mapM_ (promptAndFixOrphanedCategory conn) results

promptAndFixOrphanedCategory :: Connection -> Category -> IO ()
promptAndFixOrphanedCategory conn c = do
  printf "  [%d] %s  (owner: %s)\n" (categoryId c) (categoryName c) (categoryOwner c)
  putStr "  Delete? [y/N]: "
  hFlush stdout
  yes <- promptYN
  when yes $ do
    deleteOrphanedCategory conn (categoryId c)
    putStrLn "  Deleted."

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

-- New integrity reports

reportStaleTransactions :: MenuAction
reportStaleTransactions conn = do
  results <- selectStaleOutstandingTransactions conn
  printf "\n--- Stale outstanding/future transactions (%d) ---\n" (length results)
  mapM_ (promptAndFixStaleTransaction conn) results

promptAndFixStaleTransaction :: Connection -> Transaction -> IO ()
promptAndFixStaleTransaction conn t = do
  printTransaction t
  putStr "  Mark cleared? [y/N]: "
  hFlush stdout
  yes <- promptYN
  when yes $ do
    updateTransactionStateToCleared conn (transactionTransactionId t)
    putStrLn "  Cleared."

reportClearedFutureDate :: MenuAction
reportClearedFutureDate conn = do
  results <- selectClearedFutureDateTransactions conn
  printf "\n--- Cleared transactions with future date (%d) ---\n" (length results)
  mapM_ printTransaction results

reportActiveOnInactiveAccounts :: MenuAction
reportActiveOnInactiveAccounts conn = do
  results <- selectActiveTransactionsOnInactiveAccounts conn
  printf "\n--- Active transactions on inactive accounts (%d) ---\n" (length results)
  mapM_ printTransaction results

reportUncategorized :: MenuAction
reportUncategorized conn = do
  results <- selectUncategorizedTransactions conn
  printf "\n--- Uncategorized transactions (%d) ---\n" (length results)
  mapM_ printTransaction results

reportUndefinedType :: MenuAction
reportUndefinedType conn = do
  results <- selectUndefinedTransactionType conn
  printf "\n--- Transactions with undefined type (%d) ---\n" (length results)
  mapM_ printTransaction results

reportZeroAmount :: MenuAction
reportZeroAmount conn = do
  results <- selectZeroAmountTransactions conn
  printf "\n--- Zero-amount transactions (%d) ---\n" (length results)
  mapM_ (promptAndDeactivateTransaction conn) results

promptAndDeactivateTransaction :: Connection -> Transaction -> IO ()
promptAndDeactivateTransaction conn t = do
  printTransaction t
  putStr "  Deactivate? [y/N]: "
  hFlush stdout
  yes <- promptYN
  when yes $ do
    deactivateTransaction conn (transactionTransactionId t)
    putStrLn "  Deactivated."

reportZeroAmountInactive :: MenuAction
reportZeroAmountInactive conn = do
  results <- selectZeroAmountInactiveTransactions conn
  printf "\n--- Zero-amount inactive transactions (%d) ---\n" (length results)
  mapM_ (promptAndDeleteTransaction conn) results

promptAndDeleteTransaction :: Connection -> Transaction -> IO ()
promptAndDeleteTransaction conn t = do
  printTransaction t
  putStr "  Delete? [y/N]: "
  hFlush stdout
  yes <- promptYN
  when yes $ do
    deleteTransactionCascade conn t
    putStrLn "  Deleted."

reportZeroAmountInactiveOnActiveAccounts :: MenuAction
reportZeroAmountInactiveOnActiveAccounts conn = do
  results <- selectZeroAmountInactiveTransactionsOnActiveAccounts conn
  printf "\n--- Zero-amount inactive transactions on active accounts (%d) ---\n" (length results)
  mapM_ (promptAndDeleteTransaction conn) results

reportDanglingPayments :: MenuAction
reportDanglingPayments conn = do
  results <- selectDanglingPayments conn
  printf "\n--- Dangling payments (%d) ---\n" (length results)
  mapM_ printPayment results

reportDanglingTransfers :: MenuAction
reportDanglingTransfers conn = do
  results <- selectDanglingTransfers conn
  printf "\n--- Dangling transfers (%d) ---\n" (length results)
  mapM_ printTransfer results

reportMismatchedAccountType :: MenuAction
reportMismatchedAccountType conn = do
  results <- selectTransactionsWithMismatchedAccountType conn
  printf "\n--- Transactions with mismatched account type (%d) ---\n" (length results)
  mapM_ printTransaction results

reportNeverValidatedAccounts :: MenuAction
reportNeverValidatedAccounts conn = do
  results <- selectAccountsNeverValidated conn
  printf "\n--- Accounts never validated (%d) ---\n" (length results)
  mapM_ printAccount results

reportAccountsNoTransactions :: MenuAction
reportAccountsNoTransactions conn = do
  results <- selectAccountsWithNoTransactions conn
  printf "\n--- Active accounts with no transactions (%d) ---\n" (length results)
  mapM_ printAccount results

reportDescriptionsWhitespace :: MenuAction
reportDescriptionsWhitespace conn = do
  results <- selectDescriptionsWithWhitespace conn
  printf "\n--- Descriptions with leading/trailing spaces (%d) ---\n" (length results)
  mapM_ (\d -> printf "  [%d] |%s|  (owner: %s)\n"
    (descriptionId d) (descriptionName d) (descriptionOwner d)) results

reportOldPendingTransactions :: MenuAction
reportOldPendingTransactions conn = do
  results <- selectPendingTransactionsOlderThan30Days conn
  printf "\n--- Pending transactions older than 30 days (%d) ---\n" (length results)
  mapM_ printPendingTransaction results

reportAccountTotalsOutOfSync :: MenuAction
reportAccountTotalsOutOfSync conn = do
  results <- selectAccountTotalsOutOfSync conn
  printf "\n--- Account totals out of sync (%d) ---\n" (length results)
  mapM_ (\r -> do
    printf "  %-35s\n" (atdAccountNameOwner r)
    printf "    cleared:     stored=%s  computed=%s\n" (show (atdStoredCleared r))     (show (atdComputedCleared r))
    printf "    outstanding: stored=%s  computed=%s\n" (show (atdStoredOutstanding r)) (show (atdComputedOutstanding r))
    printf "    future:      stored=%s  computed=%s\n" (show (atdStoredFuture r))      (show (atdComputedFuture r))
    ) results

reportAccountValidationOutOfSync :: MenuAction
reportAccountValidationOutOfSync conn = do
  results <- selectAccountValidationDateOutOfSync conn
  printf "\n--- Account validation date out of sync (%d) ---\n" (length results)
  mapM_ (\r -> printf "  %-35s  stored: %s  actual: %s\n"
    (avmAccountNameOwner r)
    (show (avmStoredValidationDate r))
    (show (avmActualValidationDate r))) results

reportInactiveDescriptionsUsed :: MenuAction
reportInactiveDescriptionsUsed conn = do
  results <- selectInactiveDescriptionsStillUsed conn
  printf "\n--- Inactive descriptions still used in active transactions (%d) ---\n" (length results)
  mapM_ (\d -> printf "  [%d] %s  (owner: %s)\n"
    (descriptionId d) (descriptionName d) (descriptionOwner d)) results

reportInactiveCategoriesUsed :: MenuAction
reportInactiveCategoriesUsed conn = do
  results <- selectInactiveCategoriesStillUsed conn
  printf "\n--- Inactive categories still used in active transactions (%d) ---\n" (length results)
  mapM_ (\c -> printf "  [%d] %s  (owner: %s)\n"
    (categoryId c) (categoryName c) (categoryOwner c)) results

reportNegativeAmounts :: MenuAction
reportNegativeAmounts conn = do
  results <- selectTransactionsWithNegativeAmount conn
  printf "\n--- Transactions with negative amount (%d) ---\n" (length results)
  mapM_ printTransaction results

reportUnreasonableDate :: MenuAction
reportUnreasonableDate conn = do
  results <- selectTransactionsWithUnreasonableDate conn
  printf "\n--- Transactions with unreasonable date (%d) ---\n" (length results)
  mapM_ printTransaction results

reportOrphanedTransactionCategories :: MenuAction
reportOrphanedTransactionCategories conn = do
  results <- selectOrphanedTransactionCategories conn
  printf "\n--- Orphaned transaction-category links (%d) ---\n" (length results)
  mapM_ (\r -> printf "  category_id: %d  transaction_id: %d  owner: %s\n"
    (tcCategoryId r) (tcTransactionId r) (tcOwner r)) results

reportSelfReferencingPayments :: MenuAction
reportSelfReferencingPayments conn = do
  results <- selectSelfReferencingPayments conn
  printf "\n--- Self-referencing payments (%d) ---\n" (length results)
  mapM_ printPayment results

reportSelfReferencingTransfers :: MenuAction
reportSelfReferencingTransfers conn = do
  results <- selectSelfReferencingTransfers conn
  printf "\n--- Self-referencing transfers (%d) ---\n" (length results)
  mapM_ printTransfer results

reportUnpaidMedicalExpenses :: MenuAction
reportUnpaidMedicalExpenses conn = do
  results <- selectUnpaidMedicalExpenses conn
  printf "\n--- Unpaid medical expenses (%d) ---\n" (length results)
  mapM_ (\m -> printf "  [%d] %s  billed: %s  patient: %s  paid: %s  status: %s\n"
    (medicalExpenseId m)
    (show (medicalExpenseServiceDate m))
    (show (medicalExpenseBilledAmount m))
    (show (medicalExpensePatientResponsibility m))
    (show (medicalExpensePaidAmount m))
    (medicalExpenseClaimStatus m)) results

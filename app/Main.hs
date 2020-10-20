{-# LANGUAGE OverloadedStrings #-}

module Main where

import Finance
import Controller
import System.Exit
import System.Environment


main :: IO ()
main = do
  putStrLn "--- separated ---"
  programName <- getProgName
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args then
    print (programName ++ " help") >> exitSuccess
  else if "-v" `elem` args || "--version" `elem` args then
    print (programName ++ "version") >> exitSuccess
  else
    print "running main program..."
  putStrLn "--- separated ---"
  postgresqlUsername <- lookupEnv "POSTGRESQL_USESRNAME"
  postgresqlPassword <- lookupEnv "POSTGRESQL_PASSWORD"
  putStrLn "--- separated ---"
  apiService

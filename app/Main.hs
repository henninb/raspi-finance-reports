{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  host     <- fromMaybe "postgresql.bhenning.com" <$> lookupEnv "POSTGRESQL_HOST"
  database <- fromMaybe "finance_db"              <$> lookupEnv "POSTGRESQL_DATABASE"
  username <- fromMaybe "henninb"                 <$> lookupEnv "POSTGRESQL_USERNAME"
  password <- fromMaybe "monday1"                 <$> lookupEnv "POSTGRESQL_PASSWORD"
  conn <- connect defaultConnectInfo
    { connectHost     = host
    , connectDatabase = database
    , connectUser     = username
    , connectPassword = password
    }
  runMenu conn

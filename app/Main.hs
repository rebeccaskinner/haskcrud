{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Database.Epicdb

data ProgramConfig = ProgramConfig { inputCSV :: Maybe FilePath
                                   , dbaseName :: FilePath
                                   , listenPort :: Int
                                   } deriving (Show, Data, Typeable)

main :: IO ()
main = do
  cfg <- cmdArgs config
  print cfg
  initDB

config = ProgramConfig { inputCSV = Nothing &= help "CSV data used to seed the database" &= typ "filename"
                       , dbaseName = ":memory:" &= help "Name of the sqlite3 database to use. Defaults to ':memory:' which runs an in-memory database" &= typ "filename"
                       , listenPort = 8080 &= help "port number to listen on for http request" &= typ "port number"
                       } &= summary "Serve data"

{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
module Main where

import System.Console.CmdArgs
import Database.Epicdb
import Data.CSV.Parser
import Data.Analysis
import Network.API
import Data.Time.Clock

data ProgramConfig = ProgramConfig { inputCSV :: FilePath
                                   , listenPort :: Int
                                   } deriving (Show, Data, Typeable)

main :: IO ()
main = do
  cfg <- cmdArgs config
  conn <- initDB =<< mkConnectionMgr
  csvData <- loadFromFile (inputCSV cfg)
  let seedData = case csvData of
                   Left _ -> []
                   Right rows -> rows
  let !dbRecords = map csvToDBRecord seedData :: [DenormalizedRow]
  finalConn <- addRows dbRecords conn
  runAPI conn (listenPort cfg)

config = ProgramConfig { inputCSV = "sampledata.csv" &= help "CSV data used to seed the database" &= typ "filename"
                       , listenPort = 8080 &= help "port number to listen on for http request" &= typ "port number"
                       } &= summary "Serve data"

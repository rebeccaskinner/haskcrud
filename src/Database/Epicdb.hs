{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Epicdb where

import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time.Clock
import Data.Maybe
import Data.Text (pack)
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent.STM.TChan

-- NB: The approach to DB persistence here is a bit weird. the
-- persist-* family of libraries seem to be designed to integrate well
-- with Yesod's TH magic and there are some quirks with breaking
-- outside of those assumptions- especiallyl under time constraints.
-- I believe that this is the weakest part of the application,
-- architecturally, due in part to an impedence mismatch between the
-- exercise application architecture and the expectations of the
-- library, and in part due to my initial limited understanding of the
-- expectations of the library.

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name String
  deriving Show
Bar
  type String
  deriving Show
Consumption
  personId PersonId
  barId BarId
  date UTCTime
  deriving Show
|]

-- Unit type that exists to be a phantom type for DBCommand
data PersistConn = PeristConn

-- NB: We could use free monads here to build up a small DSL for
-- sending DB operations through the channels.  For simplicity and
-- timliness I decided to just use a sum type for this exercise.
data DBCommand = DBInsert | DBListUsers | DBListBars | DBListConsumers deriving (Eq, Show)

data DenormalizedRow =
  DenormalizedRow { personField :: String
                  , barField    :: String
                  , timestamp   :: UTCTime
                  } deriving (Eq, Show)


-- NB: Rather than inverting control to run the rest of the
-- application inside of the in-memory database process, I
-- decided to opt for an asynchronous messaging architecture where we
-- spawn off a thread to handle DB queries.  For timliness and
-- simplicity we're using separate TChans for each of the know query
-- types.  If we expected that we'd need to do more sophisticated DB
-- queries, or might be regularly updating the schema or number of
-- queries, then we'd probably want to refactor this.  As mentioned
-- above, we could use a free monad based DSL for describing queries,
-- and it would be reasonable to respond over a single channel
-- containing something like a SQLValue type.
data DBConnectionMgr a =
  DBConnectionMgr { insertChan    :: TChan DenormalizedRow
                  , usersChan     :: TChan [String]
                  , barsChan      :: TChan [String]
                  , consumersChan :: TChan [DenormalizedRow]
                  , cmdChan       :: TChan DBCommand
                  }


mkConnectionMgr :: IO (DBConnectionMgr PersistConn)
mkConnectionMgr =
  DBConnectionMgr
  <$> newTChanIO
  <*> newTChanIO
  <*> newTChanIO
  <*> newTChanIO
  <*> newTChanIO

initDB :: String -> IO ()
initDB dbPath = runSqlite (pack dbPath) $ do
  runMigration migrateAll
  now <- liftIO getCurrentTime
  cid <- insertDenormalizedRow $ DenormalizedRow "rebecca" "tofu" now
  cid <- insertDenormalizedRow $ DenormalizedRow "rebecca" "seitan" now
  cid <- insertDenormalizedRow $ DenormalizedRow "rebecca" "carrot" now
  r <- denormalizedRows
  liftIO $ print r
  return ()

denormalizedRows = do
  rows <- selectList ([] :: [Filter Consumption]) []
  let keys = map entityKey rows
  rows' <- mapM getDenormalizedRow keys
  return $ catMaybes rows'

insertDenormalizedRow (DenormalizedRow name bar date) = do
  name' <- insert $ Person name
  bar' <- insert $ Bar bar
  insert $ Consumption name' bar' date

getDenormalizedRow cid = do
  consumption <- getEntity cid
  case consumption of
    Nothing -> return Nothing
    Just entConsumption -> do
      person <- (getEntity . consumptionPersonId . entityVal) entConsumption
      case person of
        Nothing -> return Nothing
        Just p -> do
          bar <- (getEntity . consumptionBarId . entityVal) entConsumption
          case bar of
            Nothing -> return Nothing
            Just b ->
              let bType = barType . entityVal $ b
                  pName = personName . entityVal $  p
                  cDate = consumptionDate . entityVal $ entConsumption
              in return $ Just $ DenormalizedRow pName bType cDate

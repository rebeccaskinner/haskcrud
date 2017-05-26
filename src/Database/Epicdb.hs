{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Database.Epicdb where

import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time.Clock
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Control.Monad.Trans.Reader

-- NB: The approach to DB persistence here is a bit weird. the
-- persist-* family of libraries seem to be designed to integrate well
-- with Yesod's TH magic and there are some quirks with breaking
-- outside of those assumptions- especially under time constraints.
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

-- NB: We could use free monads here to build up a small DSL for
-- sending DB operations through the channels.  For simplicity and
-- timliness I decided to just use a sum type for this exercise.
data DBCommand = DBInsert DenormalizedRow | DBListUsers | DBListBars | DBListConsumers deriving (Eq, Show)

data DenormalizedRow =
  DenormalizedRow { personField :: String
                  , barField    :: String
                  , timestamp   :: UTCTime
                  } deriving (Eq, Show)

instance ToJSON DenormalizedRow where
  toJSON (DenormalizedRow name bar time) =
    object [ "person" .= name
           , "bar" .= bar
           , "date" .= time
           ]

instance FromJSON DenormalizedRow where
  parseJSON (Object v) =
    DenormalizedRow <$>
    v .: "person" <*>
    v .: "bar" <*>
    v .: "date"
  parseJSON _ = mzero

-- Define connected and disconnected as phantom types to ensure that
-- we're only performing DB operations on an operation that's been
-- connected to a db instance
data Connected = Connected deriving (Eq, Show)
data Disconnected = Disconnected deriving (Eq, Show)

-- NB: Rather than inverting control to run the rest of the
-- application inside of the in-memory database process, I decided to
-- opt for an asynchronous messaging architecture where we spawn off a
-- thread to handle DB queries.  For timliness and simplicity we're
-- using a pair of Chan's to mimick a synchronous full-duplex
-- connection.  We could use STM here to introduce some guarentees at
-- the cost of some complexity and performance.

-- | Manage the database through a pair of Chans
data DBConnectionMgr a b =
  DBConnectionMgr { cmdChan :: Chan DBCommand
                  , respChan :: Chan DBResponse
                  , dbResponse :: b
                  } deriving (Eq)

instance Functor (DBConnectionMgr a) where
  f `fmap` (DBConnectionMgr c r v) = DBConnectionMgr c r (f v)

data DBResponse =
  InsertResponse (Key Consumption)
  | UserQueryResponse [String]
  | BarQueryResponse [String]
  | ListQueryResponse [DenormalizedRow] deriving (Eq, Show)

-- | Create a disconnected connection. It must be initialized by
-- calling initDB before it can be used
mkConnectionMgr :: IO (DBConnectionMgr Disconnected ())
mkConnectionMgr = DBConnectionMgr <$> newChan <*> newChan <*> return ()

-- | Insert a row into the database
addRow :: DenormalizedRow -> DBConnectionMgr Connected a -> IO (DBConnectionMgr Connected (Key Consumption))
addRow row (DBConnectionMgr req resp _) = do
  val <- writeChan req (DBInsert row) >> readChan resp
  case val of
    InsertResponse k -> return $ DBConnectionMgr req resp k
    _ -> fail "invalid response type"

-- | Insert serveral rows into the database, returning the key of the last inserted row
addRows :: [DenormalizedRow] -> DBConnectionMgr Connected a -> IO (DBConnectionMgr Connected (Key Consumption))
addRows !rows (DBConnectionMgr req resp _) =
  foldl (\c e -> c >>= addRow e) (return $ DBConnectionMgr req resp undefined) rows

-- | Get all the rows from the database
getRows :: DBConnectionMgr Connected a -> IO (DBConnectionMgr Connected [DenormalizedRow])
getRows (DBConnectionMgr req resp _) =  do
  val <- writeChan req DBListConsumers >> readChan resp
  case val of
    ListQueryResponse rows -> return $ DBConnectionMgr req resp rows
    _ -> fail "unexpected response type"

-- | Get a list of people from the database
getUsers :: DBConnectionMgr Connected a -> IO (DBConnectionMgr Connected [String])
getUsers (DBConnectionMgr req resp _) =  do
  val <- writeChan req DBListUsers >> readChan resp
  case val of
    UserQueryResponse rows -> return $ DBConnectionMgr req resp rows
    _ -> fail "unexpected response type"

-- | Get a list of bars from the database
getBars :: DBConnectionMgr Connected a -> IO (DBConnectionMgr Connected [String])
getBars (DBConnectionMgr req resp _) =  do
  val <- writeChan req DBListBars >> readChan resp
  case val of
    BarQueryResponse rows -> return $ DBConnectionMgr req resp rows
    _ -> fail "unexpected response type"

-- | Start up an ephemeral databse inside of an IO context, and return a connection to it
runDB :: IO (DBConnectionMgr Connected ())
runDB = mkConnectionMgr >>= initDB

-- | Start up an ephemral database inside of an IO context, and manage
-- it through the provided connection
initDB :: DBConnectionMgr Disconnected () -> IO (DBConnectionMgr Connected ())
initDB (DBConnectionMgr req resp _) = do
  let db = DBConnectionMgr req resp ()
  _ <- forkIO $ runSqlite "WAL=off :memory:" $ do
    runMigration migrateAll
    forever handleCmd
  return db
  where
    handleCmd :: (MonadIO m) => ReaderT SqlBackend m ()
    handleCmd = do
      cmd <- (liftIO . readChan) req
      case cmd of
        DBInsert i -> do
          key <- insertDenormalizedRow i
          (liftIO . writeChan resp) (InsertResponse key)
        DBListConsumers -> do
          users <- denormalizedRows
          (liftIO . writeChan resp) (ListQueryResponse users)
        DBListUsers -> do
          users <- dbUsers
          (liftIO . writeChan resp) (UserQueryResponse users)
        DBListBars -> do
          bars <- dbBars
          (liftIO . writeChan resp) (BarQueryResponse bars)
      return ()

-- | Fetch a list of rows from the database
denormalizedRows :: (MonadIO m) => ReaderT SqlBackend m [DenormalizedRow]
denormalizedRows = do
  rows <- selectList ([] :: [Filter Consumption]) []
  let keys = map entityKey rows
  rows' <- mapM getDenormalizedRow keys
  return $ catMaybes rows'

-- | Fetch a list of users from the database
dbUsers :: (MonadIO m) => ReaderT SqlBackend m [String]
dbUsers = do
  rows <- selectList ([] :: [Filter Person]) []
  let names = map (personName . entityVal) rows
  return names

-- | Fetch a list of bars from the database
dbBars :: (MonadIO m) => ReaderT SqlBackend m [String]
dbBars = do
  rows <- selectList ([] :: [Filter Bar]) []
  let types = map (barType . entityVal) rows
  return types

-- | Insert a new consumption record, creating users and bars on demand
insertDenormalizedRow :: (MonadIO m) => DenormalizedRow -> ReaderT SqlBackend m (Key Consumption)
insertDenormalizedRow (DenormalizedRow name bar date) = do
  name' <- addUserIfMissing name
  bar' <- addBarIfMissing bar
  insert $ Consumption name' bar' date

-- | Try to look up a user, if no user exists add it
addUserIfMissing :: (MonadIO m) => String -> ReaderT SqlBackend m (Key Person)
addUserIfMissing name = do
  entries <- selectList [PersonName ==. name] []
  if null entries
    then insert $ Person name
    else return $ (entityKey . head) entries

-- | Try to look up a bar, if no bar exists add it
addBarIfMissing :: (MonadIO m) => String -> ReaderT SqlBackend m (Key Bar)
addBarIfMissing kind = do
  entries <- selectList [BarType ==. kind] []
  if null entries
    then insert $ Bar kind
    else return $ (entityKey . head) entries

-- | Manually reconcile foreign keys to create a denormalized db row
getDenormalizedRow :: (MonadIO m) => Key Consumption -> ReaderT SqlBackend m (Maybe DenormalizedRow)
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

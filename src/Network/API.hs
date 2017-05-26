{-# LANGUAGE OverloadedStrings #-}


module Network.API where

import Control.Monad.IO.Class
import Web.Scotty
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Analysis
import Database.Epicdb

app :: DBConnectionMgr Connected a -> IO Application
app conn = scottyApp $ runAPI conn

runApp :: Port -> DBConnectionMgr Connected a -> IO ()
runApp port conn = scotty port $ runAPI conn

runAPI :: DBConnectionMgr Connected a -> ScottyM ()
runAPI conn =  do
  get "/records" $ do -- Added for manual testing
    rows <- liftIO $ dbResponse <$> getRows conn
    json $ rows
  get "/users" $ do
    users <- liftIO $ dbResponse <$> getUsers conn
    json $ users
  get "/bars" $ do
    users <- liftIO $ dbResponse <$> getBars conn
    json $ users
  get "/streak" $ do
    rows <- liftIO $ dbResponse <$> getRows conn
    json $ longestRun rows
  get "/bestdays" $ do
    rows <- liftIO $ dbResponse <$> getRows conn
    json $ mostPerMonth rows
  post "/add" $ do
    b <- jsonData
    row <- liftIO $ dbResponse <$> addRow b conn
    json $ row

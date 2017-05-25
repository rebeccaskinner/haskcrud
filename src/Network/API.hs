{-# LANGUAGE OverloadedStrings #-}


module Network.API where

import Control.Monad.IO.Class
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Data.Analysis
import Database.Epicdb
import Debug.Trace


runAPI :: DBConnectionMgr Connected a -> Port -> IO ()
runAPI conn port =  scotty port $ do
  get "/users" $ do
    trace "got a call to users" $ return ()
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

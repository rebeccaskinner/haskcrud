{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Network.APISpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Header
import Data.Time.Clock
import Data.Time.Format
import Database.Epicdb
import Network.API
import Data.Aeson (encode)
import Data.Maybe (fromJust)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (sampleDB >>= app) $ do
  describe "get /bars" $ do
    it "responds with 200" $ do
      get "/bars" `shouldRespondWith` 200

    it "responds with a list of bars" $ do
      get "/bars" `shouldRespondWith` barResponse

  describe "get /users" $ do
    it "responds with 200" $ do
      get "/users" `shouldRespondWith` 200

    it "responds with a list of users" $ do
      get "/users" `shouldRespondWith` userResponse

  describe "post /add" $ do
    it "responds with 200" $ do
      post "/add" (encode $ DenormalizedRow "ren" "tofu" (getTS "2015-03-01T09:00:00Z")) `shouldRespondWith` 200
    it "adds records" $ do
      post "/add" (encode $ DenormalizedRow "new" "tofu" (getTS "2015-03-01T09:00:00Z")) `shouldRespondWith` 200
      get "/users" `shouldRespondWith` userResponse'

sampleDB :: IO (DBConnectionMgr Connected (Key Consumption))
sampleDB = do
  conn <- initDB =<< mkConnectionMgr
  now <- getCurrentTime
  conn' <- addRows [ DenormalizedRow "rebecca" "tofu" now
                   , DenormalizedRow "ren" "seitan" now
                   , DenormalizedRow "george" "berry" now
                   ] conn
  return conn'

barResponse =
  let ResponseMatcher status headers body = [json|["tofu","seitan","berry"]|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

userResponse =
  let ResponseMatcher status headers body = [json|["rebecca","ren","george"]|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

userResponse' =
  let ResponseMatcher status headers body = [json|["rebecca","ren","george","new"]|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

getTS :: String -> UTCTime
getTS = fromJust . parseTimeM True defaultTimeLocale isoFormat

isoFormat :: String
isoFormat = iso8601DateFormat $ Just "%H:%M:%S%Q%Z"

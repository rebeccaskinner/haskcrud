module Database.EpicdbSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Database.Epicdb
import Data.Time.Clock
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "insert" $ do
    it "adds a row" $ do
      now <- getCurrentTime
      conn <- initDB =<< mkConnectionMgr
      rowsBeforeInsert <- dbResponse <$> getRows conn
      length rowsBeforeInsert `shouldBe` 0
      _ <- addRow (DenormalizedRow "rebecca" "seitan" now) conn
      rowsAfterInsert <- dbResponse <$> getRows conn
      length rowsAfterInsert `shouldBe` 1
    it "adds several rows" $ do
      now <- getCurrentTime
      conn <- initDB =<< mkConnectionMgr
      rowsBeforeInsert <- dbResponse <$> getRows conn
      length rowsBeforeInsert `shouldBe` 0
      let rows = [ DenormalizedRow "rebecca" "seitan" now
                 , DenormalizedRow "rebecca" "seitan" now
                 , DenormalizedRow "rebecca" "seitan" now
                 ]
      _ <- dbResponse <$> addRows rows conn
      rowsAfterInsert <- dbResponse <$> getRows conn
      length rowsAfterInsert `shouldBe` length rows

    it "returns an inserted row correctly" $ do
      now <- getCurrentTime
      let row = (DenormalizedRow "rebecca" "seitan" now)
      conn <- initDB =<< mkConnectionMgr
      rowsBeforeInsert <- dbResponse <$> getRows conn
      length rowsBeforeInsert `shouldBe` 0
      _ <- addRow row conn
      rowsAfterInsert <- (head . dbResponse) <$> getRows conn
      rowsAfterInsert `shouldBe` row
    it "creates a name only if missing" $ do
      now <- getCurrentTime
      conn <- initDB =<< mkConnectionMgr
      ((length . dbResponse) <$> getUsers conn) >>= shouldBe 0
      _ <- addRow (DenormalizedRow "rebecca" "seitan" now) conn
      (dbResponse <$> getUsers conn) >>= shouldBe ["rebecca"]
      _ <- addRow (DenormalizedRow "rebecca" "tofu" now) conn
      (dbResponse <$> getUsers conn) >>= shouldBe ["rebecca"]
      _ <- addRow (DenormalizedRow "george" "tofu" now) conn
      (sort . dbResponse <$> getUsers conn) >>= shouldBe ["george", "rebecca"]
    it "creates a bar only if missing" $ do
      now <- getCurrentTime
      conn <- initDB =<< mkConnectionMgr
      ((length . dbResponse) <$> getBars conn) >>= shouldBe 0
      _ <- addRow (DenormalizedRow "rebecca" "seitan" now) conn
      (dbResponse <$> getBars conn) >>= shouldBe ["seitan"]
      _ <- addRow (DenormalizedRow "rebecca" "tofu" now) conn
      (sort . dbResponse <$> getBars conn) >>= shouldBe ["seitan", "tofu"]
      _ <- addRow (DenormalizedRow "george" "tofu" now) conn
      (sort . dbResponse <$> getBars conn) >>= shouldBe ["seitan", "tofu"]

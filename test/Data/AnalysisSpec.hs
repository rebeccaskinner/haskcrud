{-# LANGUAGE OverloadedStrings #-}
module Data.AnalysisSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.String
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe
import System.IO.Unsafe
import Database.Epicdb
import Data.CSV.Parser
import Data.Analysis
import qualified Data.ByteString.Char8 as NB
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mostPerMonth" $ do
    it "works for a single day" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"]
      (getDay . head . mostPerMonth) example `shouldBe` 1
    it "returns the right day for a single month" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ]
      (getDay . head . mostPerMonth) example `shouldBe` 3
    it "handles multiple years" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2016-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2017-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ]
      (getDay . head . mostPerMonth) example `shouldBe` 1
    it "ignores time of day" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-01T04:00:00.000Z"
                              ,"rebecca,seitan,2015-01-01T05:50:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ]
      (getDay . head . mostPerMonth) example `shouldBe` 1
    it "works for multiple months" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-01T04:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T05:50:00.000Z"
                              ,"rebecca,seitan,2015-02-01T03:00:00.000Z"
                              ,"rebecca,seitan,2015-02-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-02-02T04:00:00.000Z"
                              ,"rebecca,seitan,2015-02-03T04:00:00.000Z"
                              ,"rebecca,seitan,2015-03-01T04:00:00.000Z"
                              ,"rebecca,seitan,2015-03-10T04:00:00.000Z"
                              ,"rebecca,seitan,2015-03-10T04:00:00.000Z"
                              ]
      (map getDay . mostPerMonth) example `shouldBe` [1,2,10]
  describe "longestRun" $ do
    it "finds a run" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-04T03:00:00.000Z"
                              ]
      (map getDay . longestRun) example `shouldBe` [1,2,3]
    it "skips a shorter run" $ do
      let example = mkRecords ["rebecca,seitan,2015-01-01T03:00:00.000Z"

                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-02T03:00:00.000Z"

                              ,"rebecca,seitan,2015-01-03T03:00:00.000Z"

                              ,"rebecca,seitan,2015-01-04T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-04T03:00:00.000Z"

                              ,"rebecca,seitan,2015-01-05T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-05T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-05T03:00:00.000Z"

                              ,"rebecca,seitan,2015-01-06T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-06T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-06T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-06T03:00:00.000Z"

                              ,"rebecca,seitan,2015-01-07T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-07T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-07T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-07T03:00:00.000Z"
                              ,"rebecca,seitan,2015-01-07T03:00:00.000Z"
                              ]
      (map getDay . longestRun) example `shouldBe` [3,4,5,6,7]

csvHeader :: B.ByteString
csvHeader = "person,meat-bar-type,date"

mkCSV :: [B.ByteString] -> B.ByteString
mkCSV = (B.intercalate "\n") . (csvHeader :)

forceRecords :: B.ByteString -> [DenormalizedRow]
forceRecords = map csvToDBRecord . extractEither . parseRecords
  where
    extractEither :: Either a b -> b
    extractEither (Right x) = x

mkRecords :: [B.ByteString] -> [DenormalizedRow]
mkRecords = forceRecords . mkCSV

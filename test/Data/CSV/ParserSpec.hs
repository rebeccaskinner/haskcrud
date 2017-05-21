{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Data.CSV.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.String
import Data.CSV.Parser
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as NB
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse UTC Time" $ do
    it "is idempotent" $ do
      let now = unsafePerformIO getCurrentTime :: UTCTime
      let expected = Just $ Timestamp now
      let actual = (parseUTCTime . NB.pack) (formatTime defaultTimeLocale isoFormat now)
      expected `shouldBe` actual
    it "returns nothing on error" $
      parseUTCTime "bad string" `shouldBe` Nothing
    it "handles non-fractional seconds" $
      parseUTCTime "2017-05-21T01:00:00Z" `shouldNotBe` Nothing
    it "handles fractional seconds" $
      parseUTCTime "2017-05-21T01:00:00.000Z" `shouldNotBe` Nothing
  describe "Parse Records" $ do
    it "returns an expected value" $ do
      let sampleRecord = "person,meat-bar-type,date\nrebecca,seitan,2015-01-01T01:00:00.000Z"
      let ts = fromJust $ parseUTCTime "2015-01-01T01:00:00Z"
      let expected = Right [CSVRecord "rebecca" "seitan" ts]
      expected `shouldBe` parseRecords sampleRecord
    it "returns the expected number of values" $ do
      let expectedCount = 10
      let header = "person,meat-bar-type,date"
      let sampleRecord = replicate expectedCount "rebecca,seitan,2015-01-01T01:00:00.000Z"
      let toParse = B.intercalate "\n" (header : sampleRecord)
      let results = parseRecords toParse
      (length <$> results) `shouldBe` Right expectedCount
    it "returns an error when missing header" $
      parseRecords "rebecca,tofu,2015-01-01T01:00:00.000Z" `shouldSatisfy` isLeft
    context "when missing fields" $ do
      it "returns an error when name missing" $ do
        let sampleRecord = "person,meat-bar-type,date\n,seitan,2015-01-01T01:00:00.000Z"
        parseRecords sampleRecord `shouldSatisfy` isLeft
      it "returns an error when bar type missing" $ do
        let sampleRecord = "person,meat-bar-type,date\nrebecca,,2015-01-01T01:00:00.000Z"
        parseRecords sampleRecord `shouldSatisfy` isLeft
      it "returns an error when timestamp  missing" $ do
        let sampleRecord = "person,meat-bar-type,date\nrebecca,seitan,,"
        parseRecords sampleRecord `shouldSatisfy` isLeft
      it "returns an error when timestamp  invalid" $ do
        let sampleRecord = "person,meat-bar-type,date\nrebecca,seitan,today,"
        parseRecords sampleRecord `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

-- NonEmptyString is not actuall a valid IsString since "" is not a
-- valid NonEmptyString, but this makes the tests a bit nicer.
instance IsString NonEmptyString where
  fromString = NonEmptyString

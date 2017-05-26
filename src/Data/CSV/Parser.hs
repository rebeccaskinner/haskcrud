{-# LANGUAGE OverloadedStrings #-}
module Data.CSV.Parser ( Timestamp (..)
                       , CSVRecord (..)
                       , NonEmptyString (..)
                       , loadFromFile
                       , parseRecords
                       , isoFormat
                       , parseUTCTime
                       ) where

import Data.Csv
import qualified Data.Vector as V
import Data.Time.Format
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as NB

-- | A parse helper for deserializing a UTC timestamp
newtype Timestamp = Timestamp UTCTime deriving (Eq, Show)

-- | A parse helper for enforcing non-emptiness of strings
newtype NonEmptyString = NonEmptyString String deriving (Eq)

-- Not using OverloadedRecordFields yet, so we have manually
-- namespaced field names here.
-- | A temporary record used during parsing. These will eventually get
-- converted into a denormalized row
data CSVRecord = CSVRecord { recordPerson :: !NonEmptyString
                           , recordBarType :: !NonEmptyString
                           , recordTimestamp :: !Timestamp
                           } deriving (Eq, Show)

instance FromNamedRecord CSVRecord where
  parseNamedRecord r =
    CSVRecord <$>
    r .: "person" <*>
    r .: "meat-bar-type" <*>
    r .: "date"

instance FromField Timestamp where
  parseField s = case parseUTCTime s of
    Nothing -> fail "unable too parse UTC time"
    Just t -> pure t

instance FromField NonEmptyString where
  parseField s =
    if NB.null s then fail "invalid empty string"
    else (pure . NonEmptyString) (NB.unpack s)

instance Show NonEmptyString where
  show (NonEmptyString s) = s

-- | Parse the raw csv data into records, disgarding the header, and
-- unpacking the vector.  We are converting an efficient vector to a
-- less efficient list here for the sake of API level simplicity given
-- that performance is not a primary concern for this project.
parseRecords :: B.ByteString -> Either String [CSVRecord]
parseRecords = ((V.toList . snd) <$>) . decodeByName

-- | Load a csv file from disk and parse it
loadFromFile :: FilePath -> IO (Either String [CSVRecord])
loadFromFile = (parseRecords <$>) . B.readFile

-- | Parse our timestamp format into a `Timestamp`
parseUTCTime :: NB.ByteString -> Maybe Timestamp
parseUTCTime = (Timestamp <$>) . tParser . NB.unpack
  where
    tParser = parseTimeM True defaultTimeLocale  isoFormat

-- | helper for specifying an ISO 8601 time format
isoFormat :: String
isoFormat = iso8601DateFormat $ Just "%H:%M:%S%Q%Z"

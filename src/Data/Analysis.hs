module Data.Analysis where

import Database.Epicdb
import Data.CSV.Parser
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Control.Arrow (second)
-- Just for testing
import System.IO.Unsafe

-- | convert a Data.CSV.Parser.CSVRecord to a
-- Database.Epidcb.Denormalizedrow in order to facilitate further
-- processing.  This is the initial transformation that's used for
-- seeding the working database with values from the CSV.
csvToDBRecord :: CSVRecord -> DenormalizedRow
csvToDBRecord (CSVRecord name kind (Timestamp t)) =
  DenormalizedRow (show name) (show kind) t

-- | Groups a list of denormalized values according to a time
-- function.  Used to group by day or month.
groupByTime :: (UTCTime -> Int) -> [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByTime tFunc lst = map listTime $ groupBy compareTime lst
  where
    listTime l = (tFunc . timestamp . head $ l, l)
    compareTime a b = (tFunc . timestamp $ a) == (tFunc . timestamp $ b)

-- | Group rows by their month (irrespective of year)
groupByMonths :: [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByMonths = groupByTime getMonth

-- | Get the month of the year from a timestsamp
getMonth :: UTCTime -> Int
getMonth t = let (_,m,_) = (toGregorian . utctDay) t in m

-- | Get the day of the month from a timestamp
getDay :: UTCTime -> Int
getDay t = let (_, _, d) = (toGregorian . utctDay) t in d

-- | Chunk the list into a list of tuples of (day-of-the-month, [records-for-that-day])
groupByDays :: [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByDays = groupByTime getDay

-- | Chunk the list into tuples of
-- (month-of-the-year, [(day-of-the-month, [records-for-that-day])])
monthsAndDays :: [DenormalizedRow] -> [(Int, [(Int, [DenormalizedRow])])]
monthsAndDays l =
  let months = groupByMonths l in
    map (second groupByDays) months

-- | select the longest sublist contained in the second tuple element
-- of the containing list
biggestList :: [(Int, [a])] ->(Int, [a])
biggestList = maximumBy largestList
  where largestList (_, l) (_, l') = compare (length l) (length l')

-- | For each month, which day had the largest meat bar consumption.
-- Returned as a list of UTCTimes, with each timestamp representing an
-- arbitrary time of day on the day of the month that had the most
-- consumption
mostPerMonth :: [DenormalizedRow] -> [UTCTime]
mostPerMonth = map ((timestamp . head . snd) . biggestList . snd) . monthsAndDays

-- | Try to parse a csv file into a set of denormalized rows
parseFile :: FilePath -> IO (Either String [DenormalizedRow])
parseFile = ((map csvToDBRecord <$>) <$>) . loadFromFile

-- | Sort a list of rows by timestamp (ascending)
sortByDate :: [DenormalizedRow] -> [DenormalizedRow]
sortByDate = sortBy dateOrd
  where
    dateOrd a b = timestamp a `compare` timestamp b

-- | group rows by day (since the epoch)
groupByDay :: [DenormalizedRow] -> [[DenormalizedRow]]
groupByDay = groupBy (\a b -> (getDay . timestamp $ a) == (getDay . timestamp $ b))

-- | Get a run starting at the current list head.
getRun :: [DenormalizedRow] -> [UTCTime]
getRun lst =
  let (firstDay:days) = (groupByDay . sortByDate) lst in
    map (timestamp . head) . unEither $ foldl takeRun (Right [firstDay]) days
  where
    takeRun ::
      Either [[DenormalizedRow]] [[DenormalizedRow]] ->
      [DenormalizedRow] ->
      Either [[DenormalizedRow]] [[DenormalizedRow]]
    takeRun (Left l) _ = Left l
    takeRun (Right l) x =
      if (length . head) l < length x then Right (x : l) else Left l
    unEither :: Either [[DenormalizedRow]] [[DenormalizedRow]] -> [[DenormalizedRow]]
    unEither (Left l) = l
    unEither (Right l) = l

-- | Scan the list for the longest of all runs and return that timeslice
longestRun :: [DenormalizedRow] -> [UTCTime]
longestRun l =
  let runs = map getRun $ filter (not . null) $ tails l in
    reverse $ maximumBy runLength runs
  where
    runLength a b = length a `compare` length b

-- | unsafely get rows from a csv file on disk.  Only to be used for
-- testing.
unsafeExtractPath :: FilePath -> [DenormalizedRow]
unsafeExtractPath = extractEither . unsafePerformIO . parseFile
  where extractEither (Right x) = x

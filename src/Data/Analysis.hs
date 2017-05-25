module Data.Analysis where

import Database.Epicdb
import Data.CSV.Parser
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Control.Arrow (second)
-- Just for testing
import System.IO.Unsafe

csvToDBRecord :: CSVRecord -> DenormalizedRow
csvToDBRecord (CSVRecord name kind (Timestamp t)) =
  DenormalizedRow (show name) (show kind) t

groupByTime :: (UTCTime -> Int) -> [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByTime tFunc lst = map listTime $ groupBy compareTime lst
  where
    listTime l = (tFunc . timestamp . head $ l, l)
    compareTime a b = (tFunc . timestamp $ a) == (tFunc . timestamp $ b)

groupByMonths :: [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByMonths = groupByTime getMonth

getMonth :: UTCTime -> Int
getMonth t = let (_,m,_) = (toGregorian . utctDay) t in m

getDay :: UTCTime -> Int
getDay t = let (_, _, d) = (toGregorian . utctDay) t in d

groupByDays :: [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByDays = groupByTime getDay

monthsAndDays :: [DenormalizedRow] -> [(Int, [(Int, [DenormalizedRow])])]
monthsAndDays l =
  let months = groupByMonths l in
    map (second groupByDays) months

biggestList :: [(Int, [a])] ->(Int, [a])
biggestList = maximumBy largestList
  where largestList (_, l) (_, l') = compare (length l) (length l')

-- For each month, which day had the largest meat bar consumption.
-- Returned as a list of UTCTimes, with each timestamp representing an
-- arbitrary time of day on the day of the month that had the most
-- consumption
mostPerMonth :: [DenormalizedRow] -> [UTCTime]
-- mostPerMonth = map (timestamp . head . snd) . map (biggestList . snd) . monthsAndDays
mostPerMonth = map ((timestamp . head . snd) . biggestList . snd) . monthsAndDays

parseFile :: FilePath -> IO (Either String [DenormalizedRow])
parseFile = ((map csvToDBRecord <$>) <$>) . loadFromFile

sortByDate :: [DenormalizedRow] -> [DenormalizedRow]
sortByDate = sortBy dateOrd
  where
    dateOrd a b = timestamp a `compare` timestamp b

groupByDay :: [DenormalizedRow] -> [[DenormalizedRow]]
groupByDay = groupBy (\a b -> (getDay . timestamp $ a) == (getDay . timestamp $ b))

-- Gets a run starting at the current list head.
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

-- Scan the list for the longest of all runs and return that timeslice
longestRun :: [DenormalizedRow] -> [UTCTime]
longestRun l =
  let runs = map getRun $ filter (not . null) $ tails l in
    maximumBy runLength runs
  where
    runLength a b = length a `compare` length b

-- Just for testing with ghci
unsafeExtractPath :: FilePath -> [DenormalizedRow]
unsafeExtractPath = extractEither . unsafePerformIO . parseFile
  where extractEither (Right x) = x

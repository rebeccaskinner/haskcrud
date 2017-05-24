module Data.Analysis where

import Database.Epicdb
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Control.Arrow (second)

groupByTime :: (UTCTime -> Int) -> [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByTime tFunc lst = map listTime $ groupBy compareTime lst
  where
    listTime l = (tFunc . timestamp . head $ l, l)
    compareTime a b = (tFunc . timestamp $ a) == (tFunc . timestamp $ b)

groupByMonths :: [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByMonths = groupByTime getMonth
  where getMonth t = let (_,m,_) = (toGregorian . utctDay) t in m

groupByDays :: [DenormalizedRow] -> [(Int, [DenormalizedRow])]
groupByDays = groupByTime getDay
  where getDay t = let (_, _, d) = (toGregorian . utctDay) t in d

monthsAndDays :: [DenormalizedRow] -> [(Int, [(Int, [DenormalizedRow])])]
monthsAndDays l =
  let months = groupByMonths l in
    map (second groupByDays) months

biggestList :: [(Int, [a])] -> Int
biggestList = fst . maximumBy largestList
  where largestList (_, l) (_, l') = compare (length l) (length l')

-- Convenient but unsafe partial functions that are only to used in tests.
module Unsafe where

import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE (nonEmpty)

import Types

-- Unsafe Day constructor. Allows tests to be more succinct.
unsafeDay :: String -> Double -> Double -> Double -> Double -> Int -> Day
unsafeDay d o h l c v = 
    fromJust $ day $ unsafeDayRaw d o h l c v

-- Unsafe DayRaw constructor. Allows tests to be more succinct.
unsafeDayRaw :: String -> Double -> Double -> Double -> Double -> Int -> DayRaw
unsafeDayRaw d o h l c v = 
    dayRaw
        (fromJust $ ymd $ d)
        (fromJust $ nonNegativeRealDouble $ o)
        (fromJust $ nonNegativeRealDouble $ h)
        (fromJust $ nonNegativeRealDouble $ l)
        (fromJust $ nonNegativeRealDouble $ c)
        (fromJust $ nonNegativeInt $ v)

-- Unsafe Stock constructor. Allows tests to be more succinct.
unsafeStock :: String -> [Day] -> Stock
unsafeStock symbol days = 
    fromJust $ stock symbol (fromJust $ NE.nonEmpty $ days)

-- Unsafe YYYYMMDD constructor. Allows tests to be more succinct.
unsafeYMD :: String -> YYYYMMDD
unsafeYMD s = fromJust $ ymd $ s

-- Unsafely extracts first 2 elements into a tuple
first2 :: [a] -> (a, a)
first2 arr = (arr !! 0, arr !! 1)
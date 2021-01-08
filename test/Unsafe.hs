-- Convenient but unsafe partial functions that are only to used in tests.
module Unsafe where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV

import Types

-- Unsafe Day constructor. Allows tests to be more succinct.
unsafeDay :: String -> Int -> Int -> Int -> Int -> Int -> Day
unsafeDay d o h l c v = 
    fromJust $ day $ unsafeDayRaw d o h l c v

-- Unsafe DayRaw constructor. Allows tests to be more succinct.
unsafeDayRaw :: String -> Int -> Int -> Int -> Int -> Int -> DayRaw
unsafeDayRaw d o h l c v = 
    dayRaw
        (fromJust $ ymd $ d)
        (fromJust $ nonNegativeCents $ o)
        (fromJust $ nonNegativeCents $ h)
        (fromJust $ nonNegativeCents $ l)
        (fromJust $ nonNegativeCents $ c)
        (fromJust $ nonNegativeCents $ v)

-- Unsafe Stock constructor. Allows tests to be more succinct.
unsafeStock :: String -> [Day] -> Stock
unsafeStock symbol days = 
    fromJust $ stock symbol (NEV.unsafeFromList $ days)

-- Unsafe YYYYMMDD constructor. Allows tests to be more succinct.
unsafeYMD :: String -> YYYYMMDD
unsafeYMD s = fromJust $ ymd $ s

unsafeNonEmptyVector :: [a] -> NEV.NonEmptyVector a
unsafeNonEmptyVector list = 
    fromJust $ NEV.fromList $ list

-- Unsafely extracts first 2 elements into a tuple
first2 :: [a] -> (a, a)
first2 arr = (arr !! 0, arr !! 1)

unsafeCents :: Int -> NonNegativeCents
unsafeCents i = fromJust $ nonNegativeCents i
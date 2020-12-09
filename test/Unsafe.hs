-- Convenient but unsafe partial functions that are only to used in tests.
module Unsafe where

import Data.Maybe (fromJust)

import Types

-- Unsafe Day constructor. Allows tests to be more succinct.
unsafeDay :: String -> Float -> Float -> Float -> Float -> Int -> Day
unsafeDay d o h l c v = 
    fromJust $ day $ unsafeDayRaw d o h l c v

-- Unsafe DayRaw constructor. Allows tests to be more succinct.
unsafeDayRaw :: String -> Float -> Float -> Float -> Float -> Int -> DayRaw
unsafeDayRaw d o h l c v = 
    dayRaw
        (fromJust $ ymd $ d)
        (fromJust $ nonNegativeRealFloat $ o)
        (fromJust $ nonNegativeRealFloat $ h)
        (fromJust $ nonNegativeRealFloat $ l)
        (fromJust $ nonNegativeRealFloat $ c)
        (fromJust $ nonNegativeInt $ v)
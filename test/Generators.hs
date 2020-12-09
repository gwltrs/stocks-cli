module Generators where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import Control.Category ((>>>))
import Data.Foldable (foldr1)

import Types

-- Produces strings that should always be accepted by the YYYYMMDD smart constructor.
goodYYYYMMDDStr :: Gen String
goodYYYYMMDDStr = 
    let 
        yyyy = (choose (0, 9) :: Gen Int) <&> show & vectorOf 4 <&> concat
        mm = (choose (1, 12) :: Gen Int) <&> show <&> leadZeros 2
        dd = (choose (1, 31) :: Gen Int) <&> show <&> leadZeros 2
        concatAll y m d = y ++ m ++ d
    in
        concatAll <$> yyyy <*> mm <*> dd

-- Produces strings that should never be accepted by the YYYYMMDD smart constructor.
badYYYYMMDDStr :: Gen String
badYYYYMMDDStr = 
    let 
        goodYYYY = (choose (0, 9999) :: Gen Int) <&> show <&> leadZeros 4
        badYYYY = (choose (-999, -1) :: Gen Int) <&> show <&> leadZeros 4
        goodMM = (choose (1, 12) :: Gen Int) <&> show <&> leadZeros 2
        badMM = (oneof [choose (-9, 0), choose (13, 99)] :: Gen Int) <&> show <&> leadZeros 2
        goodDD = (choose (1, 31) :: Gen Int) <&> show <&> leadZeros 2
        badDD = (oneof [choose (-9, 0), choose (32, 99)] :: Gen Int) <&> show <&> leadZeros 2
        -- Never all 0's
        arbIndices = suchThat (choose (0, 1) & vectorOf 3) (sum >>> (>0)) 
        asPair a b = (\c d -> [c, d]) <$> a <*> b
        concatAll i y m d = zip i [y, m, d] <&> (\t -> (snd t) !! (fst t)) & concat
        allDigits = (choose (0, 9) :: Gen Int) <&> show & listOf <&> concat
        notInt str = isNothing (readMaybe str :: Maybe Int)
        
    in
        oneof [
            -- Generating strings where at least one component (YYYY, MM, DD) is invalid
            concatAll 
                <$> arbIndices 
                <*> asPair goodYYYY badYYYY 
                <*> asPair goodMM badMM 
                <*> asPair goodDD badDD,
            -- Generating all-digit, not-8-length strings
            suchThat allDigits (\str -> length str /= 8),
            -- Generating not-integer, 8-length strings
            suchThat ((arbitrary :: Gen Char) & vectorOf 8) notInt]

-- Produces (date, open, high, low, close, volume) values that should always be accepted by the Day smart constructor.
goodDayProperties :: Gen (
    YYYYMMDD, 
    NonNegativeRealFloat, 
    NonNegativeRealFloat, 
    NonNegativeRealFloat, 
    NonNegativeRealFloat, 
    NonNegativeInt)
goodDayProperties = do
    (low, high) <- choose (0, maxFloat) 
        & vectorOf 2 
        <&> (\l -> (foldr1 min l, foldr1 min l))
    open <- choose (0, 1) <&> shiftRange (0, 1) (low, high)
    close <- choose (0, 1) <&> shiftRange (0, 1) (low, high)
    date <- goodYYYYMMDDStr <&> ymd <&> fromJust
    volume <- choose (0, maxBound) <&> nonNegativeInt <&> fromJust
    pure (
        date, 
        fromJust $ nonNegativeRealFloat $ open, 
        fromJust $ nonNegativeRealFloat $ high, 
        fromJust $ nonNegativeRealFloat $ low, 
        fromJust $ nonNegativeRealFloat $ close, 
        volume)

-- Generates Float values that aren't real including NaN and +/- Infinity.
nonRealFloat :: Gen Float
nonRealFloat =
    oneof [
        pure $ read "Infinity",
        pure $ read "-Infinity",
        pure $ read "NaN"]

instance Arbitrary Stock where
    arbitrary = do
        ASCIIString arbSymbol <- arbitrary
        arbDays <- arbitrary
        return Stock { symbol = arbSymbol, days = arbDays }

instance Arbitrary Day where
    arbitrary = do
        (d, o, h, l, c, v) <- goodDayProperties
        pure $ fromJust $ day d o h l c v

-- Adds up to leadings zeros to the string.
leadZeros :: Int -> String -> String
leadZeros n str = 
    let ys = take n str
    in replicate (n - length ys) '0' ++ ys

-- Approximate minimum value of IEEE single-precision float
minFloat :: Float
minFloat = -3.4 * 10 ** 38

-- Approximate maximum value of IEEE single-precision float
maxFloat :: Float
maxFloat = 3.4 * 10 ** 38

-- Ensures lossless conversion to and from 
-- JSON which enables roundtrip testing.
roundTo16th :: Float -> Float
roundTo16th x = ((x * 16.0) & round & realToFrac) / 16.0

-- Converts the given number from one range to another.
-- Example: rangeShift (0, 1) (100, 200) 0.5 == 150.
shiftRange :: (Float, Float) -> (Float, Float) -> Float -> Float
shiftRange (oldMin, oldMax) (newMin, newMax) oldValue = 
    -- Copied from https://stackoverflow.com/a/929107/4102858
    (((oldValue - oldMin) * (newMax - newMin)) / (oldMax - oldMin)) + newMin
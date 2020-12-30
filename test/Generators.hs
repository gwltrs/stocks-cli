{-# LANGUAGE NumericUnderscores #-}
module Generators where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import Control.Category ((>>>))
import Data.Foldable (foldr1)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Data.Sort (sortOn, uniqueSortOn)

import Predundant
import Types
import Unsafe

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

-- Produces DayRaw values that should always be accepted by the Day smart constructor.
goodDayRaw :: Gen DayRaw
goodDayRaw = do
    (low, high) <- choose (0, 10_000)
        & vectorOf 2 
        <&> (\l -> (foldr1 min l, foldr1 min l))
    open <- choose (0, 1) <&> shiftRange (0, 1) (low, high)
    close <- choose (0, 1) <&> shiftRange (0, 1) (low, high)
    date <- goodYYYYMMDDStr <&> ymd <&> fromJust
    volume <- choose (0, maxBound) <&> nonNegativeInt <&> fromJust
    pure DayRaw {
        date = date, 
        open = fromJust $ nonNegativeInt $ centsFromDollars $ open, 
        high = fromJust $ nonNegativeInt $ centsFromDollars$ high, 
        low = fromJust $ nonNegativeInt $ centsFromDollars $ low, 
        close = fromJust $ nonNegativeInt $ centsFromDollars $ close, 
        volume = volume }

-- Produces values that should always be accepted by the Stock smart constructor.
goodStockArgs :: Gen (String, NEV.NonEmptyVector Day)
goodStockArgs = do
    symbol <- listOf $ oneof [choose ('A', 'Z'), pure '^', pure '.', pure '-']
    days <- listOf1 goodDayRaw
        <&> uniqueSortOn date
        <<&>> (day >>> fromJust)
        <&> (NEV.fromList >>> fromJust)
    pure (symbol, days)

-- Produces values that should never be accepted by the Stock smart constructor.
badStockArgs :: Gen (String, NEV.NonEmptyVector Day)
badStockArgs = do
    symbol <- arbitrary
    -- Has at least 2 elements and possibly has duplicate dates
    days <- (++)
        <$> (listOf goodDayRaw <&> concatMap (replicate 2))
        <*> listOf1 goodDayRaw
        & (flip suchThat) (length >>> (>= 2))
    -- Random indices to swap elements on
    (i0, i1) <- choose (0, (length days) - 1)
        & vectorOf 2
        <&> first2
        & (flip suchThat) (\t -> fst t /= snd t)
    -- Invalid because it has out-of-order dates and/or duplicate dates
    badDays <- days
        & sortOn date
        & swap i0 i1
        <&> (day >>> fromJust)
        & NEV.fromList & fromJust & pure
    pure (symbol, badDays)

-- Generates Double values that aren't real including NaN and +/- Infinity.
nonRealDouble :: Gen Double
nonRealDouble =
    oneof [
        pure $ read "Infinity",
        pure $ read "-Infinity",
        pure $ read "NaN"]

instance Arbitrary Stock where
    arbitrary = do
        (symbol, days) <- goodStockArgs
        pure $ fromJust $ stock symbol days

instance Arbitrary Day where
    arbitrary = goodDayRaw <&> (day >>> fromJust)

-- Adds up to leadings zeros to the string.
leadZeros :: Int -> String -> String
leadZeros n str = 
    let ys = take n str
    in replicate (n - length ys) '0' ++ ys

-- Converts the given number from one range to another.
-- Example: shiftRange (0, 1) (100, 200) 0.5 == 150.
shiftRange :: (Double, Double) -> (Double, Double) -> Double -> Double
shiftRange (oldMin, oldMax) (newMin, newMax) oldValue = 
    -- Copied from https://stackoverflow.com/a/929107/4102858
    (((oldValue - oldMin) * (newMax - newMin)) / (oldMax - oldMin)) + newMin

-- Swaps two elements in a list
swap :: Int -> Int -> [a] -> [a]
swap a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take b list);
            list3 = drop (succ b) list
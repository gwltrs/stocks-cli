module Generators where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (isNothing)

-- Produces strings that should always be accepted by the YYYYMMDD smart constructor
goodYYYYMMDDStr :: Gen String
goodYYYYMMDDStr = 
    let 
        yyyy = (choose (0, 9) :: Gen Int) <&> show & vectorOf 4 <&> concat
        mm = (choose (1, 12) :: Gen Int) <&> show <&> leadZeros2
        dd = (choose (1, 31) :: Gen Int) <&> show <&> leadZeros2
        concatAll y m d = y ++ m ++ d
    in
        concatAll <$> yyyy <*> mm <*> dd

-- Produces strings that should never be accepted by the YYYYMMDD smart constructor
badYYYYMMDDStr :: Gen String
badYYYYMMDDStr = 
    let 
        yyyy = (choose (0, 9) :: Gen Int) <&> show & vectorOf 4 <&> concat
        goodMM = (choose (1, 12) :: Gen Int) <&> show <&> leadZeros2
        badMM = (oneof [pure 0, choose (13, 99)] :: Gen Int) <&> show <&> leadZeros2
        goodDD = (choose (1, 31) :: Gen Int) <&> show <&> leadZeros2
        badDD = (oneof [pure 0, choose (32, 99)] :: Gen Int) <&> show <&> leadZeros2
        allDigits = (choose (0, 9) :: Gen Int) <&> show & listOf <&> concat
        notInt str = isNothing (readMaybe str :: Maybe Int)
        nonInt8Long = suchThat ((arbitrary :: Gen Char) & vectorOf 8) notInt
        concatAll y m d = y ++ m ++ d
    in
        oneof [
            concatAll <$> yyyy <*> goodMM <*> badDD,
            concatAll <$> yyyy <*> badMM <*> goodDD,
            concatAll <$> yyyy <*> badMM <*> badDD,
            suchThat allDigits (\str -> length str /= 8),
            nonInt8Long]
        
-- Adds up to leadings zeros to the string 
leadZeros2 :: String -> String
leadZeros2 str = 
    let ys = take 2 str
    in replicate (2 - length ys) '0' ++ ys
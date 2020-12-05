{-# LANGUAGE TemplateHaskell #-}

module Types (
    CLICommand(..), 
    CLIState(..), 
    Stock(..),
    Day(..),
    YYYYMMDD,
    yyyymmdd,
    yyyymmddString
) where

import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Coerce (coerce)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)

data CLICommand = CLICommand {
    -- Whitespace-delimited text the user enters to executed the command
    -- For example, name = ["download", "data"] means the user types "download data"
    -- Name tokens should be lowercase and not have extraneous whitespace
    name :: [String],
    -- Documentation shown to the user when listing all the commands via "help".
    description :: String,
    -- Performs the IO associated with the command and if necessary updates the state.
    effect :: CLIState -> IO CLIState
}

data CLIState = CLIState {
    stocks :: [Stock]
}

data Stock = Stock {
    symbol :: String,
    -- Days should be in ascending order according to date
    days :: [Day]
} deriving (Eq, Show)

instance Arbitrary Stock where
    arbitrary = do
        ASCIIString arbSymbol <- arbitrary
        arbDays <- arbitrary
        return Stock { symbol = arbSymbol, days = arbDays }

data Day = Day {
    date :: String,
    open :: Float,
    high :: Float,
    low :: Float,
    close :: Float,
    volume :: Int
} deriving (Eq, Show)

newtype YYYYMMDD = YYYYMMDD String

-- Smart constructor for YYYYMMDD.
-- String argument must be exactly 8 characters long.
-- Year component must be integer from 0 to 9999 inclusive.
-- Month component must be integer from 1 to 12 inclusive. 
-- Day component must be integer from 1 to 31 inclusive. 
yyyymmdd :: String -> Maybe YYYYMMDD
yyyymmdd str = 
    let
        parseInt s = readMaybe s :: Maybe Int 
        between mn mx a = (mn <= a) && (a <= mx)
        yyyyValid = str & take 4 & parseInt & isJust
        mmValid = str & drop 4 & take 2 & parseInt <&> between 1 12 & fromMaybe False
        ddValid = str & drop 6 & take 2 & parseInt <&> between 1 31 & fromMaybe False
    in
        if (length str == 8) && yyyyValid && mmValid && ddValid
        then Just (YYYYMMDD str) 
        else Nothing

yyyymmddString :: YYYYMMDD -> String
yyyymmddString (YYYYMMDD str) = str

instance Arbitrary Day where
    arbitrary = 
        let
            arb0To9 = choose (0, 9) <&> (show :: Int -> String)
            -- Ensures lossless conversion to and from 
            -- JSON which enables roundtrip testing.
            roundTo16th :: Float -> Float
            roundTo16th x = ((x * 16.0) & round & realToFrac) / 16.0
        in do
            arbYYYYMMDD <- vectorOf 8 arb0To9 <&> concat
            NonNegative arbOpen <- arbitrary
            NonNegative arbHigh <- arbitrary
            NonNegative arbLow <- arbitrary
            NonNegative arbClose <- arbitrary
            NonNegative arbVol <- arbitrary
            return Day { 
                date = arbYYYYMMDD,
                open = arbOpen & roundTo16th,
                high = arbHigh & roundTo16th,
                low = arbLow & roundTo16th,
                close = arbClose & roundTo16th,
                volume = arbVol }
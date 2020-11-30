module Types where

import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))

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
    date :: String, -- YYYYMMDD
    open :: Float,
    high :: Float,
    low :: Float,
    close :: Float,
    volume :: Int
} deriving (Eq, Show)

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
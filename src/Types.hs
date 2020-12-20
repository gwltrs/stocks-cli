module Types (
    CLICommand(..), 
    CLIState(..), 
    Stock, stock, symbol, days,
    Day, day, raw,
    DayRaw(..), dayRaw,
    YYYYMMDD, ymd, str,
    NonNegativeInt, nonNegativeInt, int,
    NonNegativeRealDouble, nonNegativeRealDouble, dbl
) where

import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Coerce (coerce)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)
import Control.Category ((>>>))

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

data Stock = Stock String (NE.NonEmpty Day)
    deriving (Eq, Show)

-- Smart constructor for Stock.
-- All dates must be unique and in ascending order.
stock :: String -> NE.NonEmpty Day -> Maybe Stock
stock symbol days = 
    let 
        leftDates = NE.toList days <&> (raw >>> date >>> str)
        rightDates = tail leftDates
        datesAreGood = zipWith (<) leftDates rightDates
            & foldr1 (&&)
    in 
        if (length days == 1) || datesAreGood
        then Just (Stock symbol days)
        else Nothing

-- Gets the stock's symbol
symbol :: Stock -> String
symbol (Stock s _) = s

-- Gets the stock's days
days :: Stock -> NE.NonEmpty Day
days (Stock _ d) = d

-- Newtype that enforces the following invariants:
-- high is the maximum value in [open, high, low, close]
-- and low is the minimum value in [open, high, low, close].
newtype Day = Day DayRaw
    deriving (Eq, Show)

-- The Day type without enforced invariants
-- (besides the invariants enforced by the properties).
data DayRaw = DayRaw {
    date :: YYYYMMDD,
    open :: NonNegativeRealDouble,
    high :: NonNegativeRealDouble,
    low :: NonNegativeRealDouble,
    close :: NonNegativeRealDouble,
    volume :: NonNegativeInt
} deriving (Eq, Show)

-- Convenience constructor for DayRaw.
dayRaw ::
    YYYYMMDD 
    -> NonNegativeRealDouble 
    -> NonNegativeRealDouble 
    -> NonNegativeRealDouble 
    -> NonNegativeRealDouble 
    -> NonNegativeInt
    -> DayRaw
dayRaw d o h l c v = DayRaw {
    date = d, open = o, high = h, low = l, close = c, volume = v }

-- Extracts DayRaw from Day.
raw :: Day -> DayRaw
raw (Day dr) = dr

-- Smart constructor for Day. Asserts that 
-- high is the maximum value in [open, high, low, close]
-- and low is the minimum value in [open, high, low, close]
day :: DayRaw -> Maybe Day
day dr = 
    let 
        allDoubles = [open dr, high dr, low dr, close dr] <&> dbl
        invalidLow = (dbl $ low $ dr) > foldr1 min allDoubles
        invalidHigh = (dbl $ high $ dr) < foldr1 max allDoubles
    in
        if invalidLow || invalidHigh then
            Nothing
        else
            Just $ Day $ dr

-- Newtype that enforces that YYYYMMDD date format invariant.
newtype YYYYMMDD = YYYYMMDD String
    deriving (Eq, Show, Ord)

-- Smart constructor for YYYYMMDD.
-- String argument must be exactly 8 characters long.
-- Year component must be integer from 0 to 9999 inclusive.
-- Month component must be integer from 1 to 12 inclusive.
-- Day component must be integer from 1 to 31 inclusive.
ymd :: String -> Maybe YYYYMMDD
ymd str = 
    let
        parseInt s = readMaybe s :: Maybe Int 
        between mn mx a = (mn <= a) && (a <= mx)
        checkComponent drp tk mn mx str = str 
            & drop drp 
            & take tk 
            & parseInt 
            <&> between mn mx 
            & fromMaybe False
        yValid = checkComponent 0 4 0 9999 str
        mValid = checkComponent 4 2 1 12 str
        dValid = checkComponent 6 2 1 31 str
    in
        if (length str == 8) && yValid && mValid && dValid
        then Just (YYYYMMDD str) 
        else Nothing

-- Extracts String from YYYYMMDD.
str :: YYYYMMDD -> String
str (YYYYMMDD str) = str

-- Int newtype that disallows negative values.
newtype NonNegativeInt = NonNegativeInt Int 
    deriving (Eq, Show)

-- Smart constructor for NonNegativeInt.
nonNegativeInt :: Int -> Maybe NonNegativeInt
nonNegativeInt i = 
    if i >= 0 
    then Just $ NonNegativeInt $ i  
    else Nothing 

-- Extracts Int from NonNegativeInt.
int :: NonNegativeInt -> Int
int (NonNegativeInt i) = i

newtype NonNegativeRealDouble = NonNegativeRealDouble Double
    deriving (Eq, Show)

-- Smart constructor for NonNegativeRealDouble
nonNegativeRealDouble :: Double -> Maybe NonNegativeRealDouble
nonNegativeRealDouble d =
    if isNaN d || isInfinite d || d < 0 then
        Nothing
    else 
        Just $ NonNegativeRealDouble $ d

-- Extracts Double from NonNegativeRealDouble.
dbl :: NonNegativeRealDouble -> Double
dbl (NonNegativeRealDouble d) = d
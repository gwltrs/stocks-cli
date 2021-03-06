module Types (
    CLICommand(..), 
    CLIState(..), 
    Stock, stock, symbol, days,
    Day, day, raw,
    DayRaw(..), dayRaw,
    YYYYMMDD, ymd, str,
    NonNegativeCents, nonNegativeCents, int, dollars, centsFromDollars,
    IndyParsed(..),
    IndyFunc(..),
    Indicator(..)
) where

import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Coerce (coerce)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Data.Vector (Vector)
import Control.Category ((>>>))
import Data.Char (isDigit)
import Predundant

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
    stocks :: !(Vector Stock)
}

data Stock = Stock !String !(NEV.NonEmptyVector Day)
    deriving (Eq, Show)

-- Smart constructor for Stock.
-- All dates must be unique and in ascending order.
stock :: String -> NEV.NonEmptyVector Day -> Maybe Stock
stock symbol days = 
    let 
        leftDates = days <&> (raw >>> date >>> str)
        rightDates = NEV.tail leftDates
        datesAreGood = V.zipWith (<) (NEV.toVector leftDates) rightDates
            & foldr1 (&&)
    in 
        if (length days == 1) || datesAreGood
        then Just $! (Stock symbol days)
        else Nothing

-- Gets the stock's symbol
symbol :: Stock -> String
symbol (Stock s _) = s

-- Gets the stock's days
days :: Stock -> NEV.NonEmptyVector Day
days (Stock _ d) = d

-- Newtype that enforces the following invariants:
-- high is the maximum value in [open, high, low, close]
-- and low is the minimum value in [open, high, low, close].
newtype Day = Day DayRaw
    deriving (Eq, Show)

-- The Day type without enforced invariants
-- (besides the invariants enforced by the properties).
-- Open, high, low, close are stored as cents.
data DayRaw = DayRaw {
    date :: !YYYYMMDD,
    open :: {-# UNPACK #-} !NonNegativeCents,
    high :: {-# UNPACK #-} !NonNegativeCents,
    low :: {-# UNPACK #-} !NonNegativeCents,
    close :: {-# UNPACK #-} !NonNegativeCents,
    volume :: {-# UNPACK #-} !NonNegativeCents
} deriving (Eq, Show)

-- Convenience constructor for DayRaw.
dayRaw ::
    YYYYMMDD 
    -> NonNegativeCents 
    -> NonNegativeCents 
    -> NonNegativeCents 
    -> NonNegativeCents 
    -> NonNegativeCents
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
        allCents = [open dr, high dr, low dr, close dr] <&> int
        invalidLow = (int $ low $ dr) > foldr1 min allCents
        invalidHigh = (int $ high $ dr) < foldr1 max allCents
    in
        if invalidLow || invalidHigh then
            Nothing
        else
            Just $! Day $ dr

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

        -- Validating integers manually profiles significantly 
        -- faster than using Text.Read.readMaybe.
        yyyy = str & take 4
        mmdd = str & drop 4
        mm = mmdd & take 2
        dd = mmdd & drop 2
        m1 = mmdd !! 0
        m2 = mmdd !! 1
        d1 = dd !! 0
        d2 = dd !! 1

        yyyyValid = not $ any (not . isDigit) $ yyyy
        mmNot0 = mm /= "00"
        mmNotTooBig = m1 == '0' || (m1 == '1' && between ('0', '2') m2)
        mmValid = mmNot0 && mmNotTooBig
        ddNot0 = dd /= "00"
        ddNotTooBig = between ('0', '2') d1 || (d1 == '3' && between ('0', '1') d2)
        ddValid = ddNot0 && ddNotTooBig
        
    in
        if (length str == 8) && mmValid && ddValid && yyyyValid
        then Just $! (YYYYMMDD str) 
        else Nothing

-- Extracts String from YYYYMMDD. 
str :: YYYYMMDD -> String
str (YYYYMMDD str) = str

-- Int newtype that disallows negative values.
newtype NonNegativeCents = NonNegativeCents Int 
    deriving (Eq, Show, Ord)

-- Smart constructor for NonNegativeCents.
nonNegativeCents :: Int -> Maybe NonNegativeCents
nonNegativeCents i = 
    if i >= 0 
    then Just $! NonNegativeCents $! i  
    else Nothing 

-- Extracts Int from NonNegativeCents.
int :: NonNegativeCents -> Int
int (NonNegativeCents i) = i

-- Gets the dollar amount.
dollars :: NonNegativeCents -> Double
dollars (NonNegativeCents i) = (fromIntegral i) / 100.0

-- Converts dollars as double to cents as an int.
-- Rounds to the nearest cent.
centsFromDollars :: Double -> Int
centsFromDollars dbl = round (dbl * 100)

-- Represents the parsed indicator script.
-- Using lists here for prettier literals in tests.
data IndyParsed = IndyParsed String [Either IndyParsed Double] -- name and args
    deriving (Eq, Show)

-- An indicator function that creates an indicator or another indicator function.
-- Useful for composing indicators.
data IndyFunc = IndyFunc {
    indyFuncName :: String,
    call :: [Either Indicator Double] -> Maybe Indicator
}

-- Actual indicator used to filter the stocks.
data Indicator = Indicator {
    -- The number of past days required by the indicator to filter a stock.
    lookBehind :: Int,
    -- Returns a bool that indicates if the stock is a valid pick.
    shouldBuy :: V.Vector Day -> Bool
}
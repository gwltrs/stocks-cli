module IndyCommands where

import Types
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Predundant
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Category ((>>>))

-- Finds buy picks that only end in the last slice of each stock's day vector.
findLast :: Indicator -> V.Vector Stock -> V.Vector (String, YYYYMMDD, YYYYMMDD)
findLast ind stocks = stocks
    & V.mapMaybe (\stock ->
        let
            enoughDays = NEV.length (days stock) >= lookBehind ind
            lastNSlice = stock & days & NEV.toVector & lastN (lookBehind ind)
            buy = (shouldBuy ind) lastNSlice
        in
            if enoughDays && buy then
                Just (
                    symbol stock, 
                    V.head lastNSlice & raw & date,
                    V.last lastNSlice & raw & date)
            else
                Nothing)

-- Finds all historical buy picks with the given indicator.
findAll :: Indicator -> V.Vector Stock -> V.Vector (String, YYYYMMDD, YYYYMMDD)
findAll ind stocks = stocks
    & V.concatMap (\s -> 
        days s
            & NEV.toVector 
            & slicesOf (lookBehind ind)
            & V.filter (shouldBuy ind)
            <&> (\ds -> (
                symbol s, 
                V.head ds & raw & date,
                V.last ds & raw & date)))

-- Removes the stock if the last date is earlier than the supplied date.
-- Otherwise keeps the stock and truncates dates that are after the supplied date.
sanitizeDates :: YYYYMMDD -> Stock -> Maybe Stock
sanitizeDates d s =
    -- This search O(n) but could be O(log(n)) since the dates are sorted.
    case NEV.findIndex (raw >>> date >>> (== d)) (days s) of
        Just i -> days s 
            & NEV.slice 0 (i + 1)
            & NEV.fromVector
            >>= stock (symbol s)
        Nothing -> Nothing

-- First determines the "proper sequence" of dates by looking at each stock
-- and counting which date most often follows the previous date. Stocks are
-- split on missing-date gaps in the sequence. Extra dates that are between
-- the proper sequence are removed.
contiguousDates :: V.Vector Stock -> V.Vector Stock
contiguousDates stocks = undefined
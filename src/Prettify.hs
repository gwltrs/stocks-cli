module Prettify where

import Data.Function ((&))
import Data.List.Extra (notNull)
import Data.Functor ((<&>))
import Data.Foldable (foldr1)
import Control.Category ((>>>))

import Types

-- User-visible description of the command.
prettifyCmd :: CLICommand -> String
prettifyCmd cmd = "\"" ++ (cmd & name & unwords) ++ "\": " ++ description cmd

-- User-visible description of the stocks.
prettifyStocks :: [Stock] -> String
prettifyStocks stocks =
    let
        stocksWithDays = filter (notNull . days) stocks
        stocksLengthStr = stocksWithDays & length & show
        earliestDate = stocks 
            <&> (days >>> head >>> raw >>> date >>> str) 
            & foldr1 min & prettifyDate
        latestDate = stocks 
            <&> (days >>> last >>> raw >>> date >>> str)
            & foldr1 max & prettifyDate
    in
        if notNull stocksWithDays
        then
            stocksLengthStr ++ " stocks from " ++ earliestDate ++ " to " ++ latestDate
        else
            "0 stocks"

-- Converts date string from YYYYMMDD to MM/DD/YYYY
-- Trims leading zeros from each component
prettifyDate :: String -> String
prettifyDate date =
    let 
        trimLeadingZeros str = show (read str :: Int)
        year = date & take 4 & trimLeadingZeros
        month = date & drop 4 & take 2 & trimLeadingZeros
        day = date & drop 6 & trimLeadingZeros
    in
        month ++ "/" ++ day ++ "/" ++ year
        
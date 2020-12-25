module Prettify where

import Data.Function ((&))
import Data.List.Extra (notNull)
import Data.Functor ((<&>))
import Data.Foldable (foldr1)
import Control.Category ((>>>))
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV

import Types

-- User-visible description of the command.
prettifyCmd :: CLICommand -> String
prettifyCmd cmd = "\"" ++ (cmd & name & unwords) ++ "\": " ++ description cmd

-- User-visible description of the stocks.
prettifyStocks :: V.Vector Stock -> String
prettifyStocks stocks =
    let
        lenStr = stocks & length & show
        earliestDate = stocks 
            <&> (days >>> NEV.head >>> raw >>> date >>> str) 
            & foldr1 min & prettifyDate
        latestDate = stocks 
            <&> (days >>> NEV.last >>> raw >>> date >>> str)
            & foldr1 max & prettifyDate
    in
        if not $ V.null $ stocks
        then
            lenStr ++ " stocks from " ++ earliestDate ++ " to " ++ latestDate
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
        
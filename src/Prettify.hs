module Prettify where

import Data.Function ((&))
import Data.List.Extra (notNull)
import Data.Functor ((<&>))
import Data.Foldable (foldr1)

import Types (CLICommand(name, description), Stock(days), Day(date))

-- User-visible description of the command.
prettifyCmd :: CLICommand -> String
prettifyCmd cmd = "\"" ++ (cmd & name & unwords) ++ "\": " ++ description cmd

-- User-visible description of the stocks.
prettifyStocks :: [Stock] -> String
prettifyStocks stocks = undefined
    -- let
    --     stocksWithDays = filter (notNull . days) stocks
    --     earliestDate = stocks <&> days <&> head <&> date & foldr1 min
    --     latestDate = stocks <&> days <&> last <&> date & foldr1 max

    -- in
    --     "asdf"

-- Converts date string from YYYYMMDD to MM/DD/YYYY
-- Trims leading zeros from each component
prettifyDate :: String -> String
prettifyDate date = undefined
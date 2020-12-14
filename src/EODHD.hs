{-# LANGUAGE OverloadedStrings #-}

-- Utility functions associated with https://eodhistoricaldata.com/
module EODHD where

import Data.Function ((&))
import Data.Text (Text, unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (decode, Value (..))
import Data.Aeson.Lens
import Control.Lens
import Data.Vector (toList)
import Control.Monad

import Predundant
import Types

-- Builds the URL where all the traded symbols on the specified day can be found.
symbolsURL :: String -> YYYYMMDD -> String
symbolsURL apiKey date = 
    let
        dateStr = str date
        y = dateStr & take 4
        m = dateStr & drop 4 & take 2
        d = dateStr & drop 6 & take 2
    in
        "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token="
            ++ apiKey
            ++ "&fmt=json&date="
            ++ y ++ "-" ++ m ++ "-" ++ d

-- Builds the URL where the daily chart data for a given stock can be found. 
daysURL :: String -> String -> String -> String
daysURL apiKey year symbol =
    "https://eodhistoricaldata.com/api/eod/"
        ++ symbol
        ++ ".US?api_token="
        ++ apiKey
        ++ "&fmt=json&from="
        ++ year
        ++ "-01-01"

-- Parses the JSON from the URL built by EODHD.symbolsURL.
parseSymbols :: ByteString -> Maybe [String]
parseSymbols text = text
    -- Still looking for an entirely-lens way to do this.
    ^? _Array
    <&> toList
    <<&>> (^? key "code" . _String)
    <&> allOrNothing
    & join
    <<&>> unpack

parseDays :: ByteString -> Maybe [Day]
parseDays text = undefined
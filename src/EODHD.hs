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
import Control.Monad (join)
import Data.Scientific (toBoundedInteger)
import Data.Char (isDigit)
import qualified Data.Vector as V

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
parseSymbols :: ByteString -> Maybe (V.Vector String)
parseSymbols bStr = bStr
    ^? _Array
    <<&>> (^? key "code" . _String)
    <&> sequenceA
    & join
    <<&>> unpack

parseDays :: ByteString -> Maybe (V.Vector Day)
parseDays text = 
    let
        parseDay :: Value -> Maybe Day
        parseDay v = dayRaw
            <$> (v ^? key "date" . _String 
                <&> unpack 
                <&> filter isDigit
                >>= ymd)
            <*> (v ^? key "open" . _Double <&> centsFromDollars >>= nonNegativeCents)
            <*> (v ^? key "high" . _Double <&> centsFromDollars >>= nonNegativeCents)
            <*> (v ^? key "low" . _Double <&> centsFromDollars >>= nonNegativeCents)
            <*> (v ^? key "close" . _Double <&> centsFromDollars >>= nonNegativeCents)
            <*> ((v ^? key "volume") >>= toInt >>= nonNegativeCents)
            >>= day
    in
        text
            ^? _Array
            <<&>> parseDay
            <&> sequenceA
            & join

-- Tries to extract an Int from an Aeson.Value.
toInt :: Value -> Maybe Int
toInt val = case val of
    Number s -> toBoundedInteger s
    _ -> Nothing
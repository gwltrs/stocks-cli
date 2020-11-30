{-# LANGUAGE OverloadedStrings #-}

module StocksCompactJSON where

import Prelude hiding (length)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Category ((>>>))
import Data.Aeson (decode, encode, Value (..))
import Data.Text (Text, pack, unpack, intercalate)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (Vector, mapMaybe, toList, (!?), length)
import Data.Scientific (toBoundedInteger, toRealFloat)

import Types (Stock(..), Day(..))

-- Converts stocks to JSON format where records are
-- saved as ordered arrays instead of objects.
toStocksCompactJSON :: [Stock] -> Text
toStocksCompactJSON stocks =
    let
        toJSONTxt str = str & pack & encode & toStrict & decodeUtf8
        toJSONArray txts = 
            "[" <> (intercalate "," txts) <> "]"
        dayToTxtVals d = [
            d & date & toJSONTxt, 
            d & open & show & pack, 
            d & high & show & pack, 
            d & low & show & pack, 
            d & close & show & pack,
            d & volume & show & pack]
        dayToJSON = dayToTxtVals >>> toJSONArray 
        stockToJSON s =
            toJSONArray [s & symbol & toJSONTxt, days s <&> dayToJSON & toJSONArray]
    in
        stocks <&> stockToJSON & toJSONArray

-- Parses stocks from JSON format where records are
-- saved as ordered arrays instead of objects.
parseStocksCompactJSON :: Text -> Maybe [Stock]
parseStocksCompactJSON txt =
    let 
        toDay :: Value -> Maybe Day
        toDay v = case v of
            Array vec -> do
                date <- (vec !? 0) >>= toString
                open <- (vec !? 1) >>= toFloat
                high <- (vec !? 2) >>= toFloat
                low <- (vec !? 3) >>= toFloat
                close <- (vec !? 4) >>= toFloat
                volume <- (vec !? 5) >>= toInt
                Just Day {
                    date = date, 
                    open = open, 
                    high = high, 
                    low = low, 
                    close = close, 
                    volume = volume }
            _ -> Nothing
        toStock :: Value -> Maybe Stock
        toStock v = case v of
            Array vec -> do
                symbol <- (vec !? 0) >>= toString
                days <- (vec !? 1) >>= toVector toDay 
                Just Stock { symbol = symbol, days = toList days }
            _ -> Nothing
    in
        case (txt & encodeUtf8 & fromStrict & decode :: Maybe Value) of
            Just (Array vec) -> vec & mapMaybe toStock & toList & Just
            _ -> Nothing

-- Tries to parse a String from an Aeson.Value.
toString :: Value -> Maybe String
toString val = case val of
    String txt -> Just (unpack txt)
    _ -> Nothing

-- Tries to parse a Float from an Aeson.Value.
toFloat :: Value -> Maybe Float
toFloat val = case val of
    Number s -> Just (toRealFloat s)
    _ -> Nothing

-- Tries to parse an Int from an Aeson.Value.
toInt :: Value -> Maybe Int
toInt val = case val of
    Number s -> toBoundedInteger s
    _ -> Nothing

-- Tries to parse a Vector from an Aeson.Value.
-- Returns Nothing if at least one element fails to convert.
toVector :: (Value -> Maybe a) -> Value -> Maybe (Vector a)
toVector f val = case val of
    Array vec -> 
        let mappedVec = mapMaybe f vec
        in
            if (length mappedVec) == (length vec) 
            then Just mappedVec
            else Nothing
    _ ->
        Nothing
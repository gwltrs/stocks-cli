{-# LANGUAGE OverloadedStrings #-}

module StocksCompactJSON (toStocksCompactJSON, parseStocksCompactJSON) where

import Prelude hiding (length)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Category ((>>>))
import Data.Aeson (decode, encode, Value (..))
import Data.Text (Text, pack, unpack, intercalate)
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (Vector, mapMaybe, toList, (!?), length)
import Data.Scientific (toBoundedInteger, toRealFloat)

import Types (Stock(..), Day(..), YYYYMMDD, ymd, ymdStr)

-- Converts stocks to JSON format where records are
-- saved as ordered arrays instead of objects.
toStocksCompactJSON :: [Stock] -> Text
toStocksCompactJSON stocks =
    let
        toJSONTxt = pack >>> encode >>> toStrict >>> decodeUtf8
        toJSONArray txts = 
            "[" <> (intercalate "," txts) <> "]"
        dayToTxtVals d = [
            d & date & ymdStr & toJSONTxt, 
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
    txt 
        & encodeUtf8 
        & fromStrict 
        & (decode :: ByteString -> Maybe Value) 
        >>= toMappedVector toStock 
        <&> toList

-- Tries to extract a String from an Aeson.Value.
toString :: Value -> Maybe String
toString val = case val of
    String txt -> Just (unpack txt)
    _ -> Nothing

-- Tries to extract a Float from an Aeson.Value.
toFloat :: Value -> Maybe Float
toFloat val = case val of
    Number s -> Just (toRealFloat s)
    _ -> Nothing

-- Tries to extract an Int from an Aeson.Value.
toInt :: Value -> Maybe Int
toInt val = case val of
    Number s -> toBoundedInteger s
    _ -> Nothing

-- Tries to extract a Vector from an Aeson.Value.
toVector :: Value -> Maybe (Vector Value)
toVector val =
    case val of
        Array vec -> Just vec
        _ -> Nothing

-- Tries to extract a Vector and its elements from an Aeson.Value.
-- Returns Nothing if at least one element fails to convert.
toMappedVector :: (Value -> Maybe a) -> Value -> Maybe (Vector a)
toMappedVector f val = case val of
    Array vec -> 
        let 
            mappedVec = mapMaybe f vec
        in
            if (length mappedVec) == (length vec) 
            then Just mappedVec
            else Nothing
    _ ->
        Nothing

-- Tries to extract a Day from an Aeson.Value. 
toDay :: Value -> Maybe Day
toDay val = 
    let 
        day d o h l c v =
            Day { date = d, open = o, high = h, low = l, close = c, volume = v }
        dayFromVec vec = 
            day
                <$> (vec & (!? 0) >>= toString >>= ymd)
                <*> (vec & (!? 1) >>= toFloat)
                <*> (vec & (!? 2) >>= toFloat)
                <*> (vec & (!? 3) >>= toFloat)
                <*> (vec & (!? 4) >>= toFloat)
                <*> (vec & (!? 5) >>= toInt)
    in
        toVector val >>= dayFromVec

-- Tries to extract a Stock from an Aeson.Value.
toStock :: Value -> Maybe Stock
toStock val = 
    let 
        stock s d = Stock { symbol = s, days = toList d }
        stockFromVec vec = 
            stock 
                <$> (vec & (!? 0) >>= toString)
                <*> (vec & (!? 1) >>= toMappedVector toDay)
    in 
        toVector val >>= stockFromVec
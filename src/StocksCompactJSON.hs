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
import Control.Monad (join)
import qualified Data.List.NonEmpty as NE (nonEmpty, toList)
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Getter ((^.))

import Types

-- Converts stocks to JSON format where records are
-- saved as ordered arrays instead of objects.
toStocksCompactJSON :: [Stock] -> Text
toStocksCompactJSON stocks =
    let
        toJSONTxt = pack >>> encode >>> toStrict >>> decodeUtf8
        toJSONArray txts = 
            "[" <> (intercalate "," txts) <> "]"
        dayToTxtVals d = [
            d & raw & date & str & toJSONTxt, 
            d & raw & open & dbl & show & pack, 
            d & raw & high & dbl & show & pack, 
            d & raw & low & dbl & show & pack, 
            d & raw & close & dbl & show & pack,
            d & raw & volume & int & show & pack]
        dayToJSON = dayToTxtVals >>> toJSONArray 
        stockToJSON s =
            toJSONArray [
                s & symbol & toJSONTxt, 
                days s & NE.toList <&> dayToJSON & toJSONArray]
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
    dayRaw
        <$> (val ^? nth 0 . _String <&> unpack >>= ymd)
        <*> (val ^? nth 1 . _Double >>= nonNegativeRealDouble)
        <*> (val ^? nth 2 . _Double >>= nonNegativeRealDouble)
        <*> (val ^? nth 3 . _Double >>= nonNegativeRealDouble)
        <*> (val ^? nth 4 . _Double >>= nonNegativeRealDouble)
        <*> (val ^? nth 5 >>= toInt >>= nonNegativeInt)
        >>= day
    -- let 
    --     dayFromVec vec = 
    --         dayRaw
    --             <$> ((vec !? 0) >>= toString >>= ymd)
    --             <*> ((vec !? 1) . _Double >>= nonNegativeRealDouble)
    --             <*> ((vec !? 2) . _Double >>= nonNegativeRealDouble)
    --             <*> ((vec !? 3) . _Double >>= nonNegativeRealDouble)
    --             <*> ((vec !? 4) . _Double >>= nonNegativeRealDouble)
    --             <*> ((vec !? 5) >>= toInt >>= nonNegativeInt)
    --             >>= day
    -- in
    --     toVector val >>= dayFromVec

-- Tries to extract a Stock from an Aeson.Value.
toStock :: Value -> Maybe Stock
toStock val = 
    let 
        stockFromVec vec = 
            stock 
                <$> (vec & (!? 0) >>= toString)
                <*> (vec & (!? 1) >>= toMappedVector toDay <&> toList >>= NE.nonEmpty)
                & join
    in 
        toVector val >>= stockFromVec
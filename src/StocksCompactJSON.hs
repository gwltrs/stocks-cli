{-# LANGUAGE OverloadedStrings #-}

module StocksCompactJSON (toStocksCompactJSON, parseStocksCompactJSON) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Category ((>>>))
import Data.Aeson (decode, encode, Value (..))
import Data.Text (Text, pack, unpack, intercalate)
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Control.Monad (join)
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Getter ((^.))
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV

import Types

-- Converts stocks to JSON format where records are
-- saved as ordered arrays instead of objects.
toStocksCompactJSON :: V.Vector Stock -> Text
toStocksCompactJSON stocks =
    let
        toJSONTxt :: String -> Text
        toJSONTxt = pack >>> encode >>> toStrict >>> decodeUtf8
        toJSONArray :: V.Vector Text -> Text
        toJSONArray txts = 
            "[" 
                <> foldl1 (\a b -> a <> "," <> b) txts
                <> "]"
        dayToTxtVals :: Day -> V.Vector Text
        dayToTxtVals d = V.fromList [
            d & raw & date & str & toJSONTxt, 
            d & raw & open & dbl & show & pack, 
            d & raw & high & dbl & show & pack, 
            d & raw & low & dbl & show & pack, 
            d & raw & close & dbl & show & pack,
            d & raw & volume & int & show & pack]
        dayToJSON :: Day -> Text
        dayToJSON = dayToTxtVals >>> toJSONArray 
        stockToJSON :: Stock -> Text 
        stockToJSON s =
            toJSONArray $ V.fromList [
                s & symbol & toJSONTxt, 
                days s <&> dayToJSON & NEV.toVector & toJSONArray]
    in
        stocks <&> stockToJSON & toJSONArray

-- Parses stocks from JSON format where records are
-- saved as ordered arrays instead of objects.
parseStocksCompactJSON :: Text -> Maybe (V.Vector Stock)
parseStocksCompactJSON txt =
    txt 
        & encodeUtf8 
        & fromStrict 
        & (decode :: ByteString -> Maybe Value) 
        >>= toMappedVector toStock 

-- Tries to extract a String from an Aeson.Value.
toString :: Value -> Maybe String
toString val = case val of
    String txt -> Just $! (unpack txt)
    _ -> Nothing

-- Tries to extract an Int from an Aeson.Value.
toInt :: Value -> Maybe Int
toInt val = case val of
    Number s -> toBoundedInteger s
    _ -> Nothing

-- Tries to extract a Vector from an Aeson.Value.
toVector :: Value -> Maybe (V.Vector Value)
toVector val =
    case val of
        Array vec -> Just $! vec
        _ -> Nothing

-- Tries to extract a Vector and its elements from an Aeson.Value.
-- Returns Nothing if at least one element fails to convert.
toMappedVector :: (Value -> Maybe a) -> Value -> Maybe (V.Vector a)
toMappedVector f val = case val of
    Array vec -> 
        let 
            mappedVec = V.mapMaybe f vec
        in
            if (V.length mappedVec) == (V.length vec) 
            then Just $! mappedVec
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
                <$> (vec & (V.!? 0) >>= toString)
                <*> (vec & (V.!? 1) >>= toMappedVector toDay >>= NEV.fromVector)
                & join
    in 
        toVector val >>= stockFromVec
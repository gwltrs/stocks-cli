{-# LANGUAGE OverloadedStrings #-}

module StocksCompactCSV where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Category ((>>>))
import Data.Aeson (decode, encode, Value (..))
import Data.Text (Text, pack, unpack, intercalate, splitOn)
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Control.Monad (join)
import Data.Aeson.Lens
import Control.Lens
import Control.Lens.Getter ((^.))
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Text.Read (readMaybe)
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Either.Combinators (leftToMaybe, fromLeft', fromRight')
import Data.Maybe (fromJust)

import Predundant
import Types

-- Converts stocks to CSV.
toStocksCompactCSV :: V.Vector Stock -> Text
toStocksCompactCSV stocks = 
    let 
        dayRawCSV :: DayRaw -> Text
        dayRawCSV dr = intercalate "," $! [ 
            -- Replace show with double-conversion
            (pack $! str $! date $! dr),
            (pack $! show $! dbl $! open $! dr),
            (pack $! show $! dbl $! high $! dr),
            (pack $! show $! dbl $! low $! dr),
            (pack $! show $! dbl $! close $! dr),
            (pack $! show $! int $! volume $! dr) ]
        stockCSV :: Stock -> Text
        stockCSV s =
            pack (symbol s) <> "\n" <>
                (s &! days <&> (dayRawCSV . raw) &! NEV.toList &! intercalate "\n")
    in
        stocks <&> stockCSV &! V.toList &! intercalate "\n"

-- Splits compact CSV into tokens.
-- Exporting this enables granular debugging.
tokenizeStocksCompactCSV :: Text -> V.Vector (V.Vector Text)
tokenizeStocksCompactCSV txt = txt
    & splitOn "\n"
    &! V.fromList
    <&> (V.fromList . splitOn ",")

-- Converts tokens into useable types.
-- Exporting this enables granular debugging.
interpretStocksTokens :: V.Vector (V.Vector Text) -> Maybe (V.Vector (Either Text Day))
interpretStocksTokens tokens = tokens
    <&> (\line -> 
        if V.length line == 1
        then 
            Just $! Left $! (line V.! 0)
        else
            if V.length line == 6 
            then
                dayRaw
                    <$> (line V.! 0 &! unpack &! ymd)
                    <*> (line V.! 1 &! parseDouble >>= nonNegativeRealDouble)
                    <*> (line V.! 2 &! parseDouble >>= nonNegativeRealDouble)
                    <*> (line V.! 3 &! parseDouble >>= nonNegativeRealDouble)
                    <*> (line V.! 4 &! parseDouble >>= nonNegativeRealDouble)
                    <*> (line V.! 5 &! parseInt >>= nonNegativeInt)
                    >>= day
                    <&> Right
            else 
                Nothing)
    &! allOrNothing

finalizeStockProperties :: V.Vector (Either Text Day) -> V.Vector Stock
finalizeStockProperties props = props
    &! chunkOn isLeft
    &! V.mapMaybe (\chunk ->
        let 
            s = V.head chunk &! fromLeft' &! unpack
            d = V.tail chunk <&> fromRight'
        in
            NEV.fromVector d >>= stock s)

-- Parses stocks from CSV.
-- For performance reasons, 
parseStocksCompactCSV :: Text -> Maybe (V.Vector Stock)
parseStocksCompactCSV txt = txt
    &! tokenizeStocksCompactCSV 
    &! interpretStocksTokens 
    <&> finalizeStockProperties

parseInt :: Text -> Maybe Int
parseInt = readMaybe . unpack 

parseDouble :: Text -> Maybe Double
parseDouble = readMaybe . unpack 
{-# LANGUAGE OverloadedStrings #-}

module StocksCompactCSV (toStocksCompactCSV, parseStocksCompactCSV) where

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
import Data.Either (isRight)
import Data.Either.Combinators (leftToMaybe)

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

-- Parses stocks from CSV.
parseStocksCompactCSV :: Text -> Maybe (V.Vector Stock)
parseStocksCompactCSV txt = 
    let 
        parsed1 :: V.Vector (V.Vector Text)
        parsed1 = txt
            & splitOn "\n"
            &! V.fromList
            <&> (V.fromList . splitOn ",")
        parsed2 :: Maybe (V.Vector (Either (Int, Text) Day))
        parsed2 = parsed1
            &! imap (\i line -> 
                if V.length line == 1
                then 
                    Just $! Left $! (i, line V.! 0)
                else
                    if V.length line  == 6 
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
        indices1 :: Maybe (V.Vector Int)
        indices1 = parsed2
            <&> V.mapMaybe (\e -> leftToMaybe e <&> fst)
        indices2 :: Maybe (V.Vector (Int, Int))
        indices2 = undefined
            
        parsed3 :: Maybe (V.Vector Stock)
        parsed3 = parsed2
            >>= (\p2 -> 
                if V.null p2 || isRight (V.head p2)
                then Nothing
                else Nothing

                    )
    in 
        parsed3

    -- case ((decode :: ByteString -> Maybe Value) $! fromStrict $! encodeUtf8 $! txt) of
    --     Just (Array v) ->
    --         Just $! (V.mapMaybe toStock $! v)
    --     Nothing -> 
    --         Nothing

parseInt :: Text -> Maybe Int
parseInt = readMaybe . unpack 

parseDouble :: Text -> Maybe Double
parseDouble = readMaybe . unpack 

-- -- Tries to extract a String from an Aeson.Value.
-- toString :: Value -> Maybe String
-- toString val = case val of
--     String txt -> Just $! unpack $! txt
--     _ -> Nothing

-- -- Tries to extract an Int from an Aeson.Value.
-- toInt :: Value -> Maybe Int
-- toInt val = case val of
--     Number s -> toBoundedInteger s
--     _ -> Nothing

-- -- Tries to extract a Vector from an Aeson.Value.
-- toVector :: Value -> Maybe (V.Vector Value)
-- toVector val =
--     case val of
--         Array vec -> Just $! vec
--         _ -> Nothing

-- -- Tries to extract a Vector and its elements from an Aeson.Value.
-- -- Returns Nothing if at least one element fails to convert.
-- toMappedVector :: (Value -> Maybe a) -> Value -> Maybe (V.Vector a)
-- toMappedVector f val = case val of
--     Array vec -> 
--         let 
--             mappedVec = V.mapMaybe f vec
--         in
--             if (V.length mappedVec) == (V.length vec) 
--             then Just $! mappedVec
--             else Nothing
--     _ ->
--         Nothing

-- -- Tries to extract a Day from an Aeson.Value. 
-- toDay :: Value -> Maybe Day
-- toDay val = 
--     case val of
--         Array vec -> 
--             if length vec /= 6
--             then Nothing
--             else
--                 dayRaw
--                     <$> (vec V.! 0 ^? _String <&> unpack >>= ymd)
--                     <*> (vec V.! 1 ^? _Double >>= nonNegativeRealDouble)
--                     <*> (vec V.! 2 ^? _Double >>= nonNegativeRealDouble)
--                     <*> (vec V.! 3 ^? _Double >>= nonNegativeRealDouble)
--                     <*> (vec V.! 4 ^? _Double >>= nonNegativeRealDouble)
--                     <*> (vec V.! 5 & toInt >>= nonNegativeInt)
--                     >>= day
--         _ ->
--             Nothing  

-- -- Tries to extract a Stock from an Aeson.Value.
-- toStock :: Value -> Maybe Stock
-- toStock val = 
--     case val of
--         Array vec ->
--             if length vec /= 2
--             then Nothing
--             else
--                 stock 
--                     <$> (vec & (V.! 0) & toString)
--                     <*> (vec & (V.! 1) & toMappedVector toDay >>= NEV.fromVector)
--                     & join
--         _ ->
--             Nothing
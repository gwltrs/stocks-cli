{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module StocksCompactJSONSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text, pack)
import Data.Maybe (fromJust)

import Types
import StocksCompactJSON (toStocksCompactJSON, parseStocksCompactJSON)
import Generators

stocksCompactJSONTests :: SpecWith ()
stocksCompactJSONTests = do
    describe "StocksCompactJSON" $ do
        it "doesn't change round-tripped stocks" $  do
            property $ \s -> parseStocksCompactJSON (toStocksCompactJSON s) == Just s
    describe "StocksCompactJSON.parseStocksCompactJSON" $ do
        it "parses stocks from compact JSON" $ do
            (parseStocksCompactJSON appleCompactJSON) `shouldBe` (Just appleStock)
        it "doesn't parse invalid JSON" $ do
            -- This technically has a small chance to produce valid JSON.
            property $ \str -> parseStocksCompactJSON (pack str) == Nothing
    describe "StocksCompactJSON.toStocksCompactJSON" $ do
        it "serializes stocks into compact JSON" $ do
            (toStocksCompactJSON appleStock) `shouldBe` appleCompactJSON

appleStock :: [Stock]
appleStock = [
    Stock { 
        symbol = "AAPL",
        days = [
            Day {
                date = fromJust $ ymd $ "20200323",
                open = 57.0,
                high = 57.125,
                low = 53.25,
                close = 56.125,
                volume = fromJust $ nonNegativeInt $ 336_752_800 },
            Day {
                date = fromJust $ ymd $ "20200324",
                open = 59.125,
                high = 62.0,
                low = 58.5,
                close = 61.725,
                volume = fromJust $ nonNegativeInt $ 287_530_816 } ] } ]

appleCompactJSON :: Text
appleCompactJSON = 
    "[[\"AAPL\",\
    \[[\"20200323\",57.0,57.125,53.25,56.125,336752800],\
    \[\"20200324\",59.125,62.0,58.5,61.725,287530816]]]]"
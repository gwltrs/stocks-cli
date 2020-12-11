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
import Unsafe

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
    unsafeStock
        "AAPL"
        [
            unsafeDay "20200323" 57.0 57.125 53.25 56.125 336_752_800,
            unsafeDay "20200324" 59.125 62.0 58.5 61.725 287_530_816] ] 

appleCompactJSON :: Text
appleCompactJSON = 
    "[[\"AAPL\",\
    \[[\"20200323\",57.0,57.125,53.25,56.125,336752800],\
    \[\"20200324\",59.125,62.0,58.5,61.725,287530816]]]]"
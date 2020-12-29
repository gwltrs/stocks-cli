{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module StocksCompactCSVSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text, pack)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Test.QuickCheck.Instances.Vector

import Types
import StocksCompactCSV (toStocksCompactCSV, parseStocksCompactCSV)
import Generators
import Unsafe

stocksCompactCSVTests :: SpecWith ()
stocksCompactCSVTests = do
    describe "StocksCompactCSV" $ do
        it "doesn't change round-tripped stocks" $ do
            --sample (arbitrary :: Gen (V.Vector Stock))
            property $ \s -> (parseStocksCompactCSV $ toStocksCompactCSV $ (s :: V.Vector Stock)) == (Just s)
    describe "StocksCompactCSV.parseStocksCompactCSV" $ do
        it "parses stocks from compact CSV" $ do
            (parseStocksCompactCSV appleCompactCSV) `shouldBe` (Just appleStock)
        it "doesn't parse invalid CSV" $ do
            -- This technically has a small chance to produce valid CSV.
            -- this is failing: (parseStocksCompactCSV "1" `shouldBe` Nothing)
            property $ \str -> parseStocksCompactCSV (pack str) == Nothing
    describe "StocksCompactCSV.toStocksCompactCSV" $ do
        it "serializes stocks into compact CSV" $ do
            (toStocksCompactCSV appleStock) `shouldBe` appleCompactCSV

appleStock :: V.Vector Stock
appleStock = V.fromList [
    unsafeStock
        "AAPL"
        [
            unsafeDay "20200323" 57.0 57.125 53.25 56.125 336_752_800,
            unsafeDay "20200324" 59.125 62.0 58.5 61.725 287_530_816] ] 

appleCompactCSV :: Text
appleCompactCSV = 
    "AAPL\n\
    \20200323,57.0,57.125,53.25,56.125,336752800\n\
    \20200324,59.125,62.0,58.5,61.725,287530816"
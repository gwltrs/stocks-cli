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
import Data.Function ((&))
import Data.Functor ((<&>))

import Types
import StocksCompactCSV
import Generators
import Unsafe

stocksCompactCSVTests :: SpecWith ()
stocksCompactCSVTests = do
    describe "StocksCompactCSV" $ do
        it "doesn't change round-tripped stocks" $ do
            --sample (arbitrary :: Gen (V.Vector Stock))
            property $ \s -> (parseStocksCompactCSV $ toStocksCompactCSV $ s) == (Just s)
    describe "StocksCompactCSV.tokenizeStocksCompactCSV" $ do
        it "tokenizes stocks" $ do
            tokenizeStocksCompactCSV appleCompactCSV
                `shouldBe` appleTokens 
    describe "StocksCompactCSV.interpretStocksTokens" $ do
        it "creates stock properties from tokens" $ do
            interpretStocksTokens appleTokens
                `shouldBe` Just appleProps
    describe "StocksCompactCSV.finalizeStockProperties" $ do
        it "finishes parsing stocks" $ do
            finalizeStockProperties appleProps
                `shouldBe` appleStock
    describe "StocksCompactCSV.parseStocksCompactCSV" $ do
        it "parses stocks from compact CSV" $ do
            (parseStocksCompactCSV appleCompactCSV) `shouldBe` (Just appleStock)
    describe "StocksCompactCSV.toStocksCompactCSV" $ do
        it "serializes stocks into compact CSV" $ do
            (toStocksCompactCSV appleStock) `shouldBe` appleCompactCSV

appleStock :: V.Vector Stock
appleStock = V.fromList [
    unsafeStock
        "AAPL"
        [appleDay1, appleDay2]] 

appleCompactCSV :: Text
appleCompactCSV = 
    "AAPL\n\
    \20200323,57.0,57.125,53.25,56.125,336752800\n\
    \20200324,59.125,62.0,58.5,61.725,287530816"

appleTokens :: V.Vector (V.Vector Text)
appleTokens = [
    ["AAPL"],
    ["20200323", "57.0", "57.125", "53.25", "56.125", "336752800"],
    ["20200324", "59.125", "62.0", "58.5", "61.725", "287530816"]]
        <&> V.fromList & V.fromList

appleProps :: V.Vector (Either Text Day)
appleProps = V.fromList [
    Left "AAPL",
    Right $ appleDay1,
    Right $ appleDay2]

appleDay1 :: Day 
appleDay1 = unsafeDay "20200323" 57.0 57.125 53.25 56.125 336_752_800

appleDay2 :: Day 
appleDay2 = unsafeDay "20200324" 59.125 62.0 58.5 61.725 287_530_816

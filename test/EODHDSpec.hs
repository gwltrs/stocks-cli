{-# LANGUAGE OverloadedStrings #-}
module EODHDSpec where

import Test.Hspec
import Test.QuickCheck
import Data.ByteString.Lazy (ByteString)

import Types
import Unsafe
import EODHD (symbolsURL, daysURL, parseSymbols)

eodhdTests :: SpecWith ()
eodhdTests = do
    describe "EODHD.symbolsURL" $ do
        it "correctly builds URL" $ do
            symbolsURL "API_KEY" (unsafeYMD "20190102") `shouldBe`
                "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token=API_KEY&fmt=json&date=2019-01-02"
            symbolsURL "SECRET" (unsafeYMD "20201122") `shouldBe`
                "https://eodhistoricaldata.com/api/eod-bulk-last-day/US?api_token=SECRET&fmt=json&date=2020-11-22"
    describe "EODHD.daysURL" $ do
        it "correctly builds URL" $ do
            daysURL "API_KEY" "2019" "AAPL" `shouldBe`
                "https://eodhistoricaldata.com/api/eod/AAPL.US?api_token=API_KEY&fmt=json&from=2019-01-01"
            daysURL "SECRET" "2020" "F" `shouldBe`
                "https://eodhistoricaldata.com/api/eod/F.US?api_token=SECRET&fmt=json&from=2020-01-01"
    describe "EODHD.parseSymbols" $ do
        it "correctly parses valid JSON" $ do
            parseSymbols goodSymbolsJSON `shouldBe` Just ["A", "AA", "AAA", "AAAAX"]
            parseSymbols "[]" `shouldBe` Just []
        it "rejects invalid JSON" $ do
            parseSymbols "" `shouldBe` Nothing
            parseSymbols "[" `shouldBe` Nothing
            parseSymbols "[{\"codee\": \"A\"}]" `shouldBe` Nothing
            parseSymbols "[{\"code\": 123}]" `shouldBe` Nothing
    describe "EODHD.parseDays" $ do
        it "correctly parses valid JSON" $ do
            parsesDays good2DaysJSON `shouldBe` Just good2Days
            parsesDays good1DayJSON `shouldBe` Just good1Day
            
-- Gathered from eodhistoricaldata.com/api/eod-bulk-last-day on 12/12/2020.
goodSymbolsJSON :: ByteString
goodSymbolsJSON  = 
    "[\
    \   {\
    \      \"code\":\"A\",\
    \      \"exchange_short_name\":\"US\",\
    \      \"date\":\"2020-12-11\",\
    \      \"open\":118.67,\
    \      \"high\":119.06,\
    \      \"low\":117.29,\
    \      \"close\":118.48,\
    \      \"adjusted_close\":118.48,\
    \      \"volume\":1399500\
    \   },\
    \   {\
    \      \"code\":\"AA\",\
    \      \"exchange_short_name\":\"US\",\
    \      \"date\":\"2020-12-11\",\
    \      \"open\":23.5,\
    \      \"high\":23.59,\
    \      \"low\":22.16,\
    \      \"close\":22.84,\
    \      \"adjusted_close\":22.84,\
    \      \"volume\":6157390\
    \   },\
    \   {\
    \      \"code\":\"AAA\",\
    \      \"exchange_short_name\":\"US\",\
    \      \"date\":\"2020-12-11\",\
    \      \"open\":25.068,\
    \      \"high\":25.068,\
    \      \"low\":25.06,\
    \      \"close\":25.06,\
    \      \"adjusted_close\":25.06,\
    \      \"volume\":800\
    \   },\
    \   {\
    \      \"code\":\"AAAAX\",\
    \      \"exchange_short_name\":\"US\",\
    \      \"date\":\"2020-12-11\",\
    \      \"open\":10.81,\
    \      \"high\":10.81,\
    \      \"low\":10.81,\
    \      \"close\":10.81,\
    \      \"adjusted_close\":10.81,\
    \      \"volume\":0\
    \   }\
    \]"

good1DayJSON :: ByteString
good1DayJSON = 
    ""

good1Day :: [Day]
good1Day = undefined

good2DaysJSON :: ByteString
good2DaysJSON = 
    ""

good2Days :: [Day]
good2Days = undefined
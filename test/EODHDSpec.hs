{-# LANGUAGE OverloadedStrings #-}
module EODHDSpec where

import Test.Hspec
import Test.QuickCheck
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V

import Types
import Unsafe
import EODHD (symbolsURL, daysURL, parseSymbols, parseDays)

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
            parseSymbols goodSymbolsJSON `shouldBe` (Just $ V.fromList $ ["A", "AA", "AAA", "AAAAX"])
            parseSymbols "[]" `shouldBe` Just V.empty
        it "rejects invalid JSON" $ do
            -- TODO: implement Arbitrary ByteString instance
            parseSymbols "" `shouldBe` Nothing
            parseSymbols "[" `shouldBe` Nothing
            parseSymbols "[{\"codee\": \"A\"}]" `shouldBe` Nothing
            parseSymbols "[{\"code\": 123}]" `shouldBe` Nothing
    describe "EODHD.parseDays" $ do
        it "correctly parses valid JSON" $ do
            parseDays good2DaysJSON `shouldBe` (Just $ V.fromList $ good2Days)
            parseDays good1DayJSON `shouldBe` (Just $ V.fromList $ good1Day)
            parseDays "[]" `shouldBe` Just V.empty
        it "rejects invalid JSON" $ do
            -- TODO: implement Arbitrary ByteString instance
            parseDays "" `shouldBe` Nothing
            parseDays "}" `shouldBe` Nothing
            
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
    "[\
    \   {\
    \      \"date\":\"1999-11-18\",\
    \      \"open\":45.5,\
    \      \"high\":49.75,\
    \      \"low\":40,\
    \      \"close\":44,\
    \      \"adjusted_close\":27.25,\
    \      \"volume\":44744700\
    \   }\
    \]"

good1Day :: [Day]
good1Day = [
    unsafeDay "19991118" 4550 4975 4000 4400 44744700]

good2DaysJSON :: ByteString
good2DaysJSON = 
    "[\
    \   {\
    \      \"date\":\"2020-07-28\",\
    \      \"open\":1.751,\
    \      \"high\":2,\
    \      \"low\":1,\
    \      \"close\":1.25,\
    \      \"adjusted_close\":11,\
    \      \"volume\":333\
    \   },\
    \   {\
    \      \"date\":\"2020-07-29\",\
    \      \"open\":2.75,\
    \      \"high\":2.999,\
    \      \"low\":1.9999999,\
    \      \"close\":2.5,\
    \      \"adjusted_close\":55,\
    \      \"volume\":4444\
    \   }\
    \]"

good2Days :: [Day]
good2Days = [
    unsafeDay "20200728" 175 200 100 125 333,
    unsafeDay "20200729" 275 300 200 250 4444]
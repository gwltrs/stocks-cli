module EODHDSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import Unsafe
import EODHD (symbolsURL, daysURL)

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
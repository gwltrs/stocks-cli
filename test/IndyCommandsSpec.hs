{-# LANGUAGE OverloadedLists #-}

module IndyCommandsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Data.Function ((&))

import Types
import IndyCommands
import ValidatedLiterals
import Unsafe

indyCommandsTests :: SpecWith ()
indyCommandsTests = do
    describe "IndyCommands.findLast" $ do
        it "finds buy picks for the last day in the data" $ do
            -- Shouldn't find anything with no stocks.
            findLast
                (indLast2Days (\d1 d2 -> open d1 > open d2))
                V.empty
                    `shouldBe` V.empty
            findLast
                (indLast2Days (\d1 d2 -> open d1 > open d2)) 
                (testStocks validatedLiterals)
                    `shouldBe` V.fromList [("F", unsafeYMD "20191230", unsafeYMD "20191231")]
            findLast
                (indLast2Days (\d1 d2 -> open d1 < open d2)) 
                (testStocks validatedLiterals)
                    `shouldBe` V.fromList [("AMZN", unsafeYMD "20200930", unsafeYMD "20201001")]
            -- Shouldn't find anything with a AMZN that has only 1 day.
            findLast
                (indLast2Days (\d1 d2 -> open d1 < open d2))
                (V.fromList [unsafeStock "AMZN" [unsafeDay "20210107" 2 3 1 2 1000]])
                    `shouldBe` V.empty
    describe "IndyCommands.findAll" $ do
        it "finds buy picks for all historical days" $ do
            findAll
                (unsafeIndy "and(sma_above(20, 100), not(shift(1, sma_above(20, 100))))")
                V.empty
                    `shouldBe` V.empty
            findAll
                (unsafeIndy "and(sma_above(20, 100), not(shift(1, sma_above(20, 100))))")
                (testStocks validatedLiterals)
                    `shouldBe` V.fromList [
                        ("AMZN", unsafeYMD "20190805", unsafeYMD "20191226"),
                        ("AMZN", unsafeYMD "20191113", unsafeYMD "20200408"),
                        ("F", unsafeYMD "20190729", unsafeYMD "20191218")]
    describe "IndyCommands.sanitizeStockDates" $ do
        it "sanitizes new stocks, removes old stocks" $ do
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "A" ["20191231"])
                    `shouldBe` Nothing
            sanitizeStockDates 
                (unsafeYMD "20200101")
                (unsafeStockDates "B" ["20200101"])
                    `shouldBe` Just (unsafeStockDates "B" ["20200101"])
            sanitizeStockDates
                (unsafeYMD "20200101")
                (unsafeStockDates "C" ["20200102"])
                    `shouldBe` Nothing
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "D" ["20191230", "20191231"])
                    `shouldBe` Nothing
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "E" ["20191231", "20200101"])
                    `shouldBe` Just (unsafeStockDates "E" ["20191231", "20200101"])
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "F" ["20200101", "20200102"])
                    `shouldBe` Just (unsafeStockDates "F" ["20200101"])
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "G" ["20200102", "20200103"])
                    `shouldBe` Nothing
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "H" ["20191229", "20191230", "20191231"])
                    `shouldBe` Nothing
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "I" ["20191230", "20191231", "20200101"])
                    `shouldBe` Just (unsafeStockDates "I" ["20191230", "20191231", "20200101"])
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "J" ["20191231", "20200101", "20200102"])
                    `shouldBe` Just (unsafeStockDates "J" ["20191231", "20200101"])
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "K" ["20200101", "20200102", "20200103"])
                    `shouldBe` Just (unsafeStockDates "K" ["20200101"])
            sanitizeStockDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "K" ["20200102", "20200103", "20200104"])
                    `shouldBe` Nothing

-- Creates an indicator that signals a buy based on 2 days.
indLast2Days :: (DayRaw -> DayRaw -> Bool) -> Indicator
indLast2Days lookAtLast2Days = 
    Indicator {
        lookBehind = 2,
        shouldBuy = (\days -> lookAtLast2Days (days V.! 0 & raw) (days V.! 1 & raw))}
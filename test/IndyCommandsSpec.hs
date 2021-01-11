{-# LANGUAGE OverloadedLists #-}

module IndyCommandsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Data.Function ((&))
import Data.Char (intToDigit)
import Data.Functor ((<&>))

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
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "A" ["20191231"])
                    `shouldBe` Nothing
            sanitizeDates 
                (unsafeYMD "20200101")
                (unsafeStockDates "B" ["20200101"])
                    `shouldBe` Just (unsafeStockDates "B" ["20200101"])
            sanitizeDates
                (unsafeYMD "20200101")
                (unsafeStockDates "C" ["20200102"])
                    `shouldBe` Nothing
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "D" ["20191230", "20191231"])
                    `shouldBe` Nothing
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "E" ["20191231", "20200101"])
                    `shouldBe` Just (unsafeStockDates "E" ["20191231", "20200101"])
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "F" ["20200101", "20200102"])
                    `shouldBe` Just (unsafeStockDates "F" ["20200101"])
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "G" ["20200102", "20200103"])
                    `shouldBe` Nothing
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "H" ["20191229", "20191230", "20191231"])
                    `shouldBe` Nothing
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "I" ["20191230", "20191231", "20200101"])
                    `shouldBe` Just (unsafeStockDates "I" ["20191230", "20191231", "20200101"])
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "J" ["20191231", "20200101", "20200102"])
                    `shouldBe` Just (unsafeStockDates "J" ["20191231", "20200101"])
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "K" ["20200101", "20200102", "20200103"])
                    `shouldBe` Just (unsafeStockDates "K" ["20200101"])
            sanitizeDates 
                (unsafeYMD "20200101") 
                (unsafeStockDates "K" ["20200102", "20200103", "20200104"])
                    `shouldBe` Nothing
    describe "IndyCommands.contiguousDates" $ do
        it "splits stocks on date gaps and filters extra dates" $ do
            -- These first tests are asserting that there are no changes.
            contiguousDates [] `shouldBe` []
            contiguousDates [
                unsafeStockDates "A" ["20200101"]] 
                    `shouldBe` [
                        unsafeStockDates "A" ["20200101"]]
            contiguousDates [
                unsafeStockDates "B" ["20200101", "20200104"]] 
                    `shouldBe` [
                        unsafeStockDates "B" ["20200101", "20200104"]]
            contiguousDates [
                unsafeStockDates "C" ["20191232", "20200101", "20200104", "20200105", "20200106"],
                unsafeStockDates "E" ["20200101", "20200104", "20200105", "20200106"],
                unsafeStockDates "D" ["20200101", "20200104", "20200105", "20200106", "20200107"]] 
                    `shouldBe` [
                        unsafeStockDates "C" ["20191232", "20200101", "20200104", "20200105", "20200106"],
                        unsafeStockDates "E" ["20200101", "20200104", "20200105", "20200106"],
                        unsafeStockDates "D" ["20200101", "20200104", "20200105", "20200106", "20200107"]]
            -- E is missing 01/04/2020 now and so should be split at that point.
            contiguousDates [
                unsafeStockDates "C" ["20191232", "20200101", "20200104", "20200105", "20200106"],
                unsafeStockDates "E" ["20200101", "20200105", "20200106"],
                unsafeStockDates "D" ["20200101", "20200104", "20200105", "20200106", "20200107"]] 
                    `shouldBe` [
                        unsafeStockDates "C" ["20191232", "20200101", "20200104", "20200105", "20200106"],
                        unsafeStockDates "E" ["20200101"],
                        unsafeStockDates "E" ["20200105", "20200106"],
                        unsafeStockDates "D" ["20200101", "20200104", "20200105", "20200106", "20200107"]]  
            -- Extra dates should only be removed if they
            -- aren't the first or last date in a stock.
            -- Thus all odds except 1 and 9 should be removed.
            contiguousDates [
                unsafeStockDates "W" ([1, 2, 3, 4, 6, 7, 8] <&> d),
                unsafeStockDates "X" ([2, 4, 5, 6, 8] <&> d),
                unsafeStockDates "Y" ([2, 4, 6, 7, 8] <&> d),
                unsafeStockDates "Z" ([2, 4, 6, 8, 9] <&> d)]
                    `shouldBe` [
                        unsafeStockDates "W" ([1, 2, 4, 6, 8] <&> d),
                        unsafeStockDates "X" ([2, 4, 6, 8] <&> d),
                        unsafeStockDates "Y" ([2, 4, 6, 8] <&> d),
                        unsafeStockDates "Z" ([2, 4, 6, 8, 9] <&> d)]
            -- W and Z should get split.
            contiguousDates [
                unsafeStockDates "W" ([2, 6, 8] <&> d),
                unsafeStockDates "X" ([2, 4, 6, 8] <&> d),
                unsafeStockDates "Y" ([2, 4, 6, 8] <&> d),
                unsafeStockDates "Z" ([2, 4, 8] <&> d)] 
                    `shouldBe` [
                        unsafeStockDates "W" ([2] <&> d),
                        unsafeStockDates "W" ([6, 8] <&> d),
                        unsafeStockDates "X" ([2, 4, 6, 8] <&> d),
                        unsafeStockDates "Y" ([2, 4, 6, 8] <&> d),
                        unsafeStockDates "Z" ([2, 4] <&> d),
                        unsafeStockDates "Z" ([8] <&> d)]
            -- Testing gap-splitting and extra-filtering together
            contiguousDates [
                unsafeStockDates "W" ([1, 2, 4, 6, 8] <&> d),
                unsafeStockDates "X" ([2, 3, 4, 6, 8] <&> d),
                unsafeStockDates "Y" ([2, 4, 5, 8] <&> d),
                unsafeStockDates "Z" ([2, 5, 6, 7, 8, 9] <&> d)] 
                    `shouldBe` [
                        unsafeStockDates "W" ([1, 2, 4, 6, 8] <&> d),
                        unsafeStockDates "X" ([2, 4, 6, 8] <&> d),
                        unsafeStockDates "Y" ([2, 4] <&> d),
                        unsafeStockDates "Y" ([8] <&> d),
                        unsafeStockDates "Z" ([2] <&> d),
                        unsafeStockDates "Z" ([6, 8, 9] <&> d)] 
            
            

-- Creates an indicator that signals a buy based on 2 days.
indLast2Days :: (DayRaw -> DayRaw -> Bool) -> Indicator
indLast2Days lookAtLast2Days = 
    Indicator {
        lookBehind = 2,
        shouldBuy = (\days -> lookAtLast2Days (days V.! 0 & raw) (days V.! 1 & raw))}

-- Date-from-int. Creates a YYYYMMDD string from 1-9.
-- Allows the tests to be a little less verbose.
d :: Int -> String
d i = 
    let c = intToDigit i
    in [c, c, c, c, '0', c , '0', c]
module IndicatorParsingSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import IndicatorParsing

indicatorParsingTests :: SpecWith ()
indicatorParsingTests = do
    describe "IndicatorParsing.parseIndicators" $ do
        it "parses valid input" $ do
            -- Indicator names are alphanumeric but can also have "_".
            parseIndicators "smva5_10()"
                `shouldBe` Just (Leaf "smva5_10" [])
            -- Making sure extra parens and whitespace aren't a problem.
            parseIndicators "    ((  (IS_DAY_OF_WEEK(   -1     )    )   )  )  "
                `shouldBe` Just (Leaf "IS_DAY_OF_WEEK" [-1.0])
            parseIndicators "(a(10, 20) & b(1.5)) | ((c() & d()) | e(0.25)"
                `shouldBe` Just (
                    Or [
                        (And [Leaf "a" [10.0, 20.0], Leaf "b" [1.5]]),
                        (Or [
                            And [
                                Leaf "c" [], 
                                Leaf "d" []],
                            Leaf "e" [0.25]   
                                ])])


        it "rejects invalid inputs" $ do
            parseIndicators "" 
                `shouldBe` Nothing
            parseIndicators "some_indicator"
                `shouldBe` Nothing
            -- Indicator names must start with an alpha char.
            parseIndicators "5is_rising(0.0, 1.0)" 
                `shouldBe` Nothing
            parseIndicators "_is_rising()" 
                `shouldBe` Nothing
            -- Invalid because '|' and '&' have no precedence.
            -- Parentheses needed here.
            parseIndicators "a(10, 20) & b(1.5) | c()"
                `shouldBe` Nothing
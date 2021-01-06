module IndyParsingSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either (isLeft)

import Types
import IndyParsing (parseIndy)

indyParsingTests :: SpecWith ()
indyParsingTests = do
    describe "IndicatorParsing.parseIndicators" $ do
        it "parses valid input" $ do
            -- Indicator names are alphanumeric but can also have "_".
            parseIndy "smva5_10()"
                `shouldBe` Right (IndyParsed "smva5_10" [])
            -- Making sure extra parens and whitespace aren't a problem.
            parseIndy "  \t  IS_DAY_OF_WEEK(   -1    \n )        \n   "
                `shouldBe` Right (IndyParsed "IS_DAY_OF_WEEK" [Right (-1.0)])
            -- Realistic test.
            parseIndy 
                "\
                \and(\
                \    golden_cross(15, 20.0),\
                \    offsetBy(-5, candlestick(doji())),\
                \    volume_at_least(50000)\
                \)\
                \"
                    `shouldBe` Right (
                        IndyParsed "and" [
                            leftIndyParsed "golden_cross" [
                                Right 15.0, 
                                Right 20.0],
                            leftIndyParsed "offsetBy" [
                                Right (-5.0), 
                                leftIndyParsed "candlestick" [
                                    leftIndyParsed "doji" []]],
                            leftIndyParsed "volume_at_least" [
                                Right 50000.0]])
        it "rejects invalid inputs" $ do
            -- Expecting exactly one function at root.
            parseIndy "" `shouldSatisfy` isLeft
            -- Missing parens.
            parseIndy "some_indicator" `shouldSatisfy` isLeft
            -- Indicator names must start with an alpha char.
            parseIndy "5is_rising(0.0, 1.0)" `shouldSatisfy` isLeft
            -- Indicator names must start with an alpha char.
            parseIndy "_is_rising()" `shouldSatisfy` isLeft
            -- Expecting exactly one function at root.
            parseIndy "a(10, 20) b(1.5)" `shouldSatisfy` isLeft

-- Avoiding a pair of parens.
leftIndyParsed :: String -> [Either IndyParsed Double] -> Either IndyParsed Double
leftIndyParsed name args = Left (IndyParsed name args)
module IndyComposingSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Vector as V
import Data.Bifunctor (second)

import Types
import IndyComposing (CompositionError(..), composeIndy)
import IndyFunctions (andIF)

indyComposingTests :: SpecWith ()
indyComposingTests = do
    describe "IndyComposing.composeIndy" $ do
        it "should compose indicator" $ do
            -- Composed indicator should have a look behind of 11.
            (composeIndy composeTestFuncs composeTestFalseParsed
                <&> lookBehind)
                    `shouldBe` Right 11
            -- Composed indicator should return True.
            (composeIndy composeTestFuncs composeTestTrueParsed
                <&> (\i -> (shouldBuy i) V.empty))
                    `shouldBe` (Right True)
            -- Composed indicator should return False.
            (composeIndy composeTestFuncs composeTestFalseParsed
                <&> (\i -> (shouldBuy i) V.empty))
                    `shouldBe` (Right False)
        it "should reject invalid indy script" $ do
            -- Invalid because the no indicator functions are provided. 
            (getFuncNameNotFound $ composeIndy 
                [constIndyFunc "always" 0 True] 
                (IndyParsed "golden_cross" [Right 50.0, Right 100.0]))
                    `shouldBe` Just "golden_cross"
            -- Invalid because golden cross function is expecting 2 args.
            (getFuncWithInvalidArgs $ composeIndy 
                [constIndyFunc "always" 0 True, goldenCrossIndyFunc] 
                (IndyParsed "always" [     
                    Left $ IndyParsed "always" [Right 100.0], 
                    Left $ IndyParsed "golden_cross" [Right 100.0]]))
                        `shouldBe` Just "golden_cross"

-- Always returns "should buy" regardless of inputs.
constIndyFunc :: String -> Int -> Bool -> IndyFunc
constIndyFunc name lb res = IndyFunc name (const (Just (constIndicator lb res)))

-- Always returns "should buy" regardless of inputs.
constIndicator :: Int -> Bool -> Indicator
constIndicator lb res = Indicator { lookBehind = lb, shouldBuy = const res }

-- Test indicator function that requires exactly args to produce the always indicator.
goldenCrossIndyFunc :: IndyFunc
goldenCrossIndyFunc = IndyFunc 
    "golden_cross" 
    (\args -> if length args == 2 then Just (constIndicator 0 True) else Nothing)

-- Need this to test error case of CompositionResult since it
-- doesn't have an Eq instance due to having a function property. 
getFuncNameNotFound :: Either CompositionError Indicator -> Maybe String
getFuncNameNotFound (Left (FuncNameNotFound s)) = Just s
getFuncNameNotFound _ = Nothing

-- Need this to test error case of CompositionResult since it
-- doesn't have an Eq instance due to having a function property. 
getFuncWithInvalidArgs :: Either CompositionError Indicator -> Maybe String
getFuncWithInvalidArgs (Left (FuncWithInvalidArgs s)) = Just s
getFuncWithInvalidArgs_ = Nothing

-- Should produce a max look behind of 11.
composeTestFuncs :: [IndyFunc]
composeTestFuncs = [
    andIF, 
    constIndyFunc "yes" 5 True, 
    constIndyFunc "always" 11 True, 
    constIndyFunc "never" 7 False]

-- Should produce a result of True.
composeTestTrueParsed :: IndyParsed
composeTestTrueParsed = (IndyParsed "and" [
    Left $ IndyParsed "and" [
        Left $ IndyParsed "always" [],
        Left $ IndyParsed "yes" [Right 12.0, Right 20.0]],
    Left $ IndyParsed "yes" [Right 12.0]])

-- Should produce a result False and a max look behind of 11.
composeTestFalseParsed :: IndyParsed
composeTestFalseParsed = (IndyParsed "and" [
    Left $ IndyParsed "never" [],
    Left $ IndyParsed "always" [],
    Left $ IndyParsed "yes" [Right 12.0]])


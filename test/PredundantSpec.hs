module PredundantSpec (predundantTests) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Data.Tuple.Extra (second)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (isJust)

import Predundant (chunkOn, slicesOf, fromNestedList)

predundantTests :: SpecWith ()
predundantTests = do
    describe "Predundant.chunkOn" $ do
        it "correctly creates chunks" $ do
            "" `inChunkOnShouldBe` []
            "a" `inChunkOnShouldBe` []
            "bc" `inChunkOnShouldBe` []
            "0" `inChunkOnShouldBe` ["0"]
            "1a" `inChunkOnShouldBe` ["1a"]
            "asdf1a" `inChunkOnShouldBe` ["1a"]
            "2bc" `inChunkOnShouldBe` ["2bc"]
            " 2bc" `inChunkOnShouldBe` ["2bc"]
            "34x56y78" `inChunkOnShouldBe` ["3", "4x", "5", "6y", "7", "8"]
            "w34x56y78" `inChunkOnShouldBe` ["3", "4x", "5", "6y", "7", "8"]
            "0xyz1" `inChunkOnShouldBe` ["0xyz", "1"]
            "abc0xyz1" `inChunkOnShouldBe` ["0xyz", "1"]
    describe "Predundant.slicesOf" $ do
        it "correctly slices vector" $ do
            shouldSliceTo 1 "" []
            shouldSliceTo 2 "" []
            shouldSliceTo 3 "" []
            shouldSliceTo 1 "a" ["a"]
            shouldSliceTo 2 "a" []
            shouldSliceTo 3 "a" []
            shouldSliceTo 1 "ab" ["a", "b"]
            shouldSliceTo 2 "ab" ["ab"]
            shouldSliceTo 3 "ab" []
            shouldSliceTo 1 "abc" ["a", "b", "c"]
            shouldSliceTo 2 "abc" ["ab", "bc"]
            shouldSliceTo 3 "abc" ["abc"]
            shouldSliceTo 1 "abcd" ["a", "b", "c", "d"]
            shouldSliceTo 2 "abcd" ["ab", "bc", "cd"]
            shouldSliceTo 3 "abcd" ["abc", "bcd"]
            shouldSliceTo 0 "abc" []
            shouldSliceTo (-1) "abc" []
            shouldSliceTo (-99) "" []
        where
            inChunkOnShouldBe :: [Char] -> [[Char]] -> Expectation
            inChunkOnShouldBe given expected = 
                V.toList (chunkOn isInt $ V.fromList given)
                    `shouldBe` (expected <&> V.fromList)
            shouldSliceTo :: Int -> [Char] -> [[Char]] -> Expectation
            shouldSliceTo givenInt givenList expectedList =
                let
                    expected :: V.Vector (V.Vector Char)
                    expected = fromNestedList expectedList
                    actual :: V.Vector (V.Vector Char)
                    actual = slicesOf givenInt (V.fromList givenList)
                in
                    actual `shouldBe` expected

isInt :: Char -> Bool
isInt c = (readMaybe [c] :: Maybe Int) 
    & isJust
module PredundantSpec (predundantTests) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Data.Tuple.Extra (second)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (isJust)

import Predundant (chunkOn)

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
        where
            inChunkOnShouldBe :: [Char] -> [[Char]] -> Expectation
            inChunkOnShouldBe given expected = 
                V.toList (chunkOn isInt $ V.fromList given)
                    `shouldBe` (expected <&> V.fromList)

isInt :: Char -> Bool
isInt c = (readMaybe [c] :: Maybe Int) 
    & isJust
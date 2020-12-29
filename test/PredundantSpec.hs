module PredundantSpec (predundantTests) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Data.Tuple.Extra (second)
import Data.Function ((&))
import Data.Functor ((<&>))

import Predundant (chunkOn)

predundantTests :: SpecWith ()
predundantTests = do
    describe "Predundant.chunkOn" $ do
        it "correctly creates chunks" $ do
            "" `inChunkOnShouldBe` []
            "a" `inChunkOnShouldBe` []
            "bc" `inChunkOnShouldBe` []
            "0" `inChunkOnShouldBe` [(0, "")]
            "1a" `inChunkOnShouldBe` [(1, "a")]
            "asdf1a" `inChunkOnShouldBe` [(1, "a")]
            "2bc" `inChunkOnShouldBe` [(2, "bc")]
            " 2bc" `inChunkOnShouldBe` [(2, "bc")]
            "34x56y78" `inChunkOnShouldBe` [(3, ""), (4, "x"), (5, ""), (6, "y"), (7, ""), (8, "")]
            "w34x56y78" `inChunkOnShouldBe` [(3, ""), (4, "x"), (5, ""), (6, "y"), (7, ""), (8, "")]
            "0xyz1" `inChunkOnShouldBe` [(0, "xyz"), (1, "")]
            "abc0xyz1" `inChunkOnShouldBe` [(0, "xyz"), (1, "")]
        where
            inChunkOnShouldBe :: [Char] -> [(Int, [Char])] -> Expectation
            inChunkOnShouldBe given expected = 
                V.toList (chunkOn parseInt $ V.fromList given) 
                    `shouldBe` (expected <&> second V.fromList)

parseInt :: Char -> Maybe Int
parseInt = readMaybe . pure
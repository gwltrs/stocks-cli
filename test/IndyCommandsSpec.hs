module IndyCommandsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Data.Function ((&))

import Types
import IndyCommands (findLast)
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

indLast2Days :: (DayRaw -> DayRaw -> Bool) -> Indicator
indLast2Days lookAtLast2Days = 
    Indicator {
        lookBehind = 2,
        shouldBuy = (\days -> lookAtLast2Days (days V.! 0 & raw) (days V.! 1 & raw))}
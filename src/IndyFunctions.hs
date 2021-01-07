module IndyFunctions where

import Data.Either.Combinators (leftToMaybe)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import Data.Function ((&))

import Predundant
import Types

andIF :: IndyFunc
andIF = IndyFunc {
    indyFuncName = "and",
    call = (\args -> 
        let 
            indicatorArgs = mapMaybe leftToMaybe args
            maxLookBehind :: Int
            maxLookBehind = indicatorArgs 
                <&> lookBehind 
                & foldr max 0
        in
            if length indicatorArgs == 0
            then Nothing
            else 
                Just Indicator {
                    lookBehind = maxLookBehind,
                    shouldBuy = (\days -> indicatorArgs 
                        <&> (\ind -> (shouldBuy ind) (lastN (lookBehind ind) days))
                        & and)})}
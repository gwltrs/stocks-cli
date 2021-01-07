module IndyFunctions (andIF) where
import Data.Either.Combinators (leftToMaybe)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import Data.Function ((&))

import Predundant
import Types

-- Produces an indicator that signals a buy only 
-- if all the indicator arguments also signal a buy.
andIF :: IndyFunc
andIF = IndyFunc {
    indyFuncName = "and",
    call = (\args -> 
        let 
            indicatorArgs = mapMaybe leftToMaybe args
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
module IndyFunctions (andIF, orIF) where
import Data.Either.Combinators (leftToMaybe)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import Data.Function ((&))

import Predundant
import Types

-- Produces an indicator that signals a buy only 
-- if all the indicator arguments also signal a buy.
andIF :: IndyFunc
andIF = booleanImpl "and" (&&)

-- Produces an indicator that signals a buy if at least 
-- one of the given indicator arguments signals a buy.
orIF :: IndyFunc
orIF = booleanImpl "or" (||)

-- Avoiding boilerplate. "and" and "or" IndyFuncs are almost identical.
booleanImpl :: String -> (Bool -> Bool -> Bool) -> IndyFunc
booleanImpl name boolOp = 
    IndyFunc {
        indyFuncName = name,
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
                            & foldr boolOp True)})}
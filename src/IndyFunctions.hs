module IndyFunctions (allIndyFuncs, andIF, orIF) where
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import Data.Function ((&))
import qualified Data.Vector as V
import Control.Category ((>>>))

import Predundant
import Types

-- Exporting all the indy functions that are 
-- ready for use along with documentation text.
allIndyFuncs :: [(IndyFunc, String)]
allIndyFuncs = [
    (
        andIF, 
        "Accepts indicator args.\n\
        \Returns an indicator that signals a buy when\n\
        \all the indicator args also signal a buy."),
    (
        orIF,
        "Accepts indicator args.\n\
        \Returns an indicator that signals a buy when\n\
        \all the indicator args also signal a buy."),
    (   
        smaAboveIF,
        "Accepts 2 numeric args.\n\
        \Signals a buy if the simple moving average with\n\
        \the range of the first arg is greater than the second.")]

-- Produces an indicator that signals a buy only 
-- if all the indicator arguments also signal a buy.
andIF :: IndyFunc
andIF = booleanImpl "and" (&&)

-- Produces an indicator that signals a buy if at least 
-- one of the given indicator arguments signals a buy.
orIF :: IndyFunc
orIF = booleanImpl "or" (||)

-- Accepts 2 numeric args.
-- Signals a buy if the simple moving average with
-- the range of the first arg is greater than the second.
smaAboveIF :: IndyFunc
smaAboveIF = IndyFunc {
    indyFuncName = "sma_above",
    call = (\args -> 
        case args <&> rightToMaybe <&> (>>= posInt) & sequenceA of
            Just [int1, int2] -> 
                Just Indicator {
                    lookBehind = max int1 int2,
                    shouldBuy = (\days -> 
                        let 
                            sma n = (days 
                                & lastN n 
                                <&> (raw >>> close >>> dollars)
                                & foldr1 (+))
                                / fromIntegral n
                        in
                            sma int1 > sma int2
                        
                        )}
            _ -> Nothing)}


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

-- Many indicator args actually want a 
-- positive integer instead of a float.
posInt :: Double -> Maybe Int
posInt dbl = 
    if round dbl > 0 
    then Just (round dbl)
    else Nothing
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
        notIF,
        "Accepts 1 indicator arg.\n\
        \Returns a new indicator that signals a buy only\n\
        \when the given indicator arg doesn't signal a buy."),
    ( 
        shiftIF,
        "Accepts 1 positive integer arg (N) and 1 indicator arg, in that order.\n\
        \Returns an indicator that uses the same signaling logic as\n\
        \the given indicator but starts looking N days earlier."),
    (   
        smaAboveIF,
        "Accepts 2 positive integer args.\n\
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

-- Accepts 1 indicator arg.
-- Returns a new indicator that signals a buy only
-- when the given indicator arg doesn't signal a buy.
notIF :: IndyFunc
notIF = IndyFunc {
    indyFuncName = "not",
    call = (\args-> 
        case args of 
            [Left indArg] -> 
                Just Indicator {
                    lookBehind = lookBehind indArg,
                    shouldBuy = not . (shouldBuy indArg)}
            _ -> Nothing)}

-- Accepts 1 positive integer arg (N) and 1 indicator arg, in that order.
-- Returns an indicator that uses the same signaling logic as
-- the given indicator but starts looking N days earlier.
shiftIF :: IndyFunc
shiftIF = IndyFunc {
    indyFuncName = "shift",
    call = (\args ->
        case args of
            [arg1, arg2] ->
                case pair <$> (rightToMaybe arg1 >>= posInt) <*> (leftToMaybe arg2) of
                    Just (n, ind) -> 
                        Just Indicator {
                            lookBehind = n + (lookBehind ind),
                            shouldBuy = (\days -> days
                                & V.take (lookBehind ind)
                                & (shouldBuy ind))}
                    _ -> Nothing
            _ -> Nothing)}

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

-- Many indicators actually want positive integers instead of floats.
posInt :: Double -> Maybe Int
posInt dbl = 
    if round dbl > 0 
    then Just (round dbl)
    else Nothing
module Predundant where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Control.Category ((>>>))
import qualified Data.Vector as V
import Data.Function ((&))
import Data.Either.Combinators (rightToMaybe)

-- Returns array if all elements are Just.
-- Otherwise returns Nothing.
allOrNothing :: V.Vector (Maybe a) -> Maybe (V.Vector a)
allOrNothing arr =
    let 
        filtered = V.mapMaybe id arr
    in 
        if V.length filtered == V.length arr 
        then Just filtered
        else Nothing

-- Returns the last n elements of the give vector.
-- Will return the vector unchanged if length <= n.
lastN :: Int -> V.Vector a -> V.Vector a
lastN n vec = 
    if V.length vec <= n
    then vec
    else V.drop (V.length vec - n) vec

-- Maps a doubly-nested functor.
infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) f map = f <&> (<&> map)
{-# INLINE (<<&>>) #-}

-- Strict evaluation version of Data.Function.(&)
infixl 1 &!
(&!) :: a -> (a -> b) -> b
(&!) a f = f $! a
{-# INLINE (&!) #-}

-- Returns a bool indicating if the value is between the range, inclusive.
-- Behavior is undefined for max < min.
between :: Ord a => (a, a) -> a -> Bool
between (min, max) value =
    min <= value && value <= max

-- Creates chunksof the vector using the given predicate.
-- A new chunk is created whenever an element satisfies the given predicate.
-- The first chunk doesn't start until the first 
-- element that satisfies the predicate is found.
chunkOn :: (a -> Bool) -> V.Vector a -> V.Vector (V.Vector a)
chunkOn isHead vec =
    let 
        pair a b = (a, b)
        mapped1 :: V.Vector Int
        mapped1 = vec
            & V.imapMaybe (\i e -> if isHead e then Just i else Nothing)
        mapped2 = mapped1 
            & V.imap (\i e -> 
                let
                    isLast = i == (length mapped1 - 1)
                    sliceInds = (
                        e, 
                        if isLast 
                        then length vec - 1
                        else mapped1 V.! (i + 1) - 1)
                in
                    V.slice (fst sliceInds) (snd sliceInds - fst sliceInds + 1) vec)
    in
        mapped2

-- Returns a list of length 1 that contains the given argument.
singleton :: a -> [a]
singleton a = [a] 

-- Converts Maybe to Either.
toEither :: err -> Maybe a -> Either err a
toEither err Nothing = Left err
toEither _ (Just a) = Right a
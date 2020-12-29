module Predundant where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Control.Category ((>>>))
import qualified Data.Vector as V
import Data.Function ((&))

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

-- Maps a doubly-nested functor.
infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) f map = f <&> (<&> map)
{-# INLINE (<<&>>) #-}

infixl 1 &!
(&!) :: a -> (a -> b) -> b
(&!) a f = f $! a
{-# INLINE (&!) #-}

-- Returns a bool indicating if the value is between the range, inclusive.
-- Behavior is undefined for max < min.
between :: Ord a => (a, a) -> a -> Bool
between (min, max) value =
    min <= value && value <= max

-- Creates chunks (head, tail) of the vector using the given predicate.
-- A new chunk is created whenever an element satisfies the given predicate.
-- The first chunk doesn't start until the first 
-- element that satisfies the predicate is created.
chunkOn :: (a -> Maybe b) -> V.Vector a -> V.Vector (b, V.Vector a)
chunkOn mapHeadMaybe vec = 
    let 
        pair a b = (a, b)
        mapped1 = vec
            & V.imapMaybe (\i e -> mapHeadMaybe e <&> pair i)
        mapped2 = mapped1 
            & V.imap (\i tup -> 
                let
                    isLast = i == (length mapped1 - 1)
                    sliceInds = (
                        fst tup + 1, 
                        if isLast 
                        then length vec - 1
                        else (mapped1 V.! (i + 1) & fst) - 1)
                in
                    (snd tup, 
                    V.slice (fst sliceInds) (snd sliceInds - fst sliceInds + 1) vec))
    in
        mapped2
module Predundant where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Control.Category ((>>>))
import qualified Data.Vector as V

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

-- Returns a bool indicating if the value is between the range, inclusive.
-- Behavior is undefined for max < min.
between :: Ord a => (a, a) -> a -> Bool
between (min, max) value =
    min <= value && value <= max
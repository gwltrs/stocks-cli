module Predundant where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)

-- Maps doubly-nested Functors.
infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) a f = a <&> (<&> f)
{-# INLINE (<<&>>) #-}

-- Returns array if all elements are Just.
-- Otherwise returns Nothing.
allOrNothing :: [Maybe a] -> Maybe [a]
allOrNothing arr =
    let 
        filtered = mapMaybe id arr
    in 
        if length filtered == length arr 
        then Just filtered
        else Nothing
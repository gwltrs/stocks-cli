module Predundant where

import Data.Functor ((<&>))

-- Maps doubly-nested Functors.
infixl 1 <<&>>
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) a f = a <&> (<&> f)
{-# INLINE (<<&>>) #-}
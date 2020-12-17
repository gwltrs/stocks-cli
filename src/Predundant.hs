module Predundant where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Control.Category ((>>>))



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


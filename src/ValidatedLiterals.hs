-- A non-template-haskell derivation of the validated-literals 
-- library where unwraps are checked by tests
module ValidatedLiterals where

import Data.Maybe (fromJust)
import Data.Function ((&))

import Types (Stock)
import TestStocks (testStocksCompactJSON)
import StocksCompactJSON (parseStocksCompactJSON)

data ValidatedLiterals = ValidatedLiterals {
    testStocks :: [Stock]
} deriving (Eq, Show)

validatedLiterals :: ValidatedLiterals
validatedLiterals =
    let makeLiterals ts = ValidatedLiterals { testStocks = ts }
    in makeLiterals 
        <$> parseStocksCompactJSON testStocksCompactJSON
        & fromJust
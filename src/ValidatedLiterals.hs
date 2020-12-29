-- A non-template-haskell derivation of the validated-literals 
-- library where unwraps are checked by tests
module ValidatedLiterals where

import Data.Maybe (fromJust)
import Data.Function ((&))
import qualified Data.Vector as V

import Types (Stock)
import TestStocks (testStocksCompactCSV)
import StocksCompactCSV (parseStocksCompactCSV)

data ValidatedLiterals = ValidatedLiterals {
    testStocks :: V.Vector Stock
} deriving (Eq, Show)

validatedLiterals :: ValidatedLiterals
validatedLiterals =
    let makeLiterals ts = ValidatedLiterals { testStocks = ts }
    in makeLiterals 
        <$> parseStocksCompactCSV testStocksCompactCSV
        & fromJust
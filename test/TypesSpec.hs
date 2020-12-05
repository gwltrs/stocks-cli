module TypesSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Functor ((<&>))
import Data.Function ((&))

import Generators (goodYYYYMMDDStr, badYYYYMMDDStr)
import Types (yyyymmdd, YYYYMMDD(..), yyyymmddString)

typesTests :: SpecWith ()
typesTests = do
    describe "Types.yyyymmdd" $ do
        it "should create YYYYMMDD when given valid string" $ do
            property
                $ forAll goodYYYYMMDDStr 
                $ (\str -> ((yyyymmdd str) <&> yyyymmddString) == Just str)
        it "should fail when given invalid string" $ do
            property
                $ forAll badYYYYMMDDStr 
                $ (\str -> ((yyyymmdd str) <&> yyyymmddString) == Nothing)



module ValidatedLiteralsSpec where

import Test.Hspec
import Test.QuickCheck

import ValidatedLiterals (validatedLiterals)

validatedLiteralsTests :: SpecWith ()
validatedLiteralsTests = do
    describe "ValidatedLiterals.validatedLiterals" $ do
        it "successfully validated" $ do
            -- Just checking that it doesn't throw when evaluation is forced
            validatedLiterals `shouldSatisfy` (\vl -> (length $ show $ vl) > -1)
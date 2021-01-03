module IndicatorParsing where

import Text.Parsec as P
import Control.Applicative
import Control.Monad.Identity (Identity)

import Types

-- Attempts to parse the indicator-describing, command-line input.
parseIndicators :: String -> Maybe IndicatorScript
parseIndicators = 
    let

        integer :: Parsec String () String
        integer = P.many1 P.digit

        decimals :: Parsec String () String
        decimals = (++) <$> pure ['.'] <*> integer

        float :: Parsec String () String
        float = (\a b c -> a ++ b ++ c) 
            <$> option "" (pure "-")
            <*> integer
            <*> option "" decimals

        

        --decimals :: Parsec String () [Char]
        --decimals = 

        -- int = digit >>= P.many1
        -- parseInt = many1 

        -- signed =
    in
        undefined




module IndyParsing (parseIndy) where

import Prelude hiding (and, or)
import Predundant
import Text.Parsec as P
import Text.Parsec.Char as P
import Control.Applicative
import Control.Monad.Identity (Identity)
import Text.Read (readMaybe)
import Data.Functor ((<&>))
import Data.Char (ord, isDigit, isSpace)
import Data.Function ((&))
import Data.Either.Extra (mapLeft)

import Types

-- Parses the indicator "Indy" script.
-- Check tests for examples.
parseIndy :: String -> Either String IndyParsed
parseIndy sourceStr =
    let
        soureWOSpaces = sourceStr & filter (not . isSpace)
    in
        P.runP (fCall <* P.eof) () "" soureWOSpaces
            & mapLeft show

-- Parser for a signed floating point number with optional decimals. 
float :: Parsec String () Double
float = 
    let 
        integer = P.many1 P.digit
        decimals = (++) <$> P.string "." <*> integer
    in
        (\a b c -> a ++ b ++ c) 
            <$> option "" (P.string "-")
            <*> integer
            <*> option "" decimals
            <&> (readMaybe :: String -> Maybe Double)
            >>= maybe (unexpected "Invalid floating point value") pure

-- Parser for function names.
-- Names are alphanumeric-with-underscore and start with an alpha.
fName :: Parsec String () String
fName = (++)
    <$> (P.satisfy isASCIIAlpha <&> singleton)
    <*> (P.many $ P.satisfy isASCIIAlphaNumUnderscore)

-- Parser for function call. Arguments can be floats are other function calls.
fCall :: Parsec String () IndyParsed
fCall =
    let 
        floatsInParens = P.choice [float <&> Right, fCall <&> Left]
            & (flip P.sepBy) (P.char ',') 
            & P.between (P.char '(') (P.char ')')
    in
        IndyParsed <$> fName <*> floatsInParens

-- Get the character's unicode code point.
toCode :: Char -> Int
toCode = fromIntegral . ord

-- Returns a bool indicating if the character is a ASCII-alphabetical.
isASCIIAlpha :: Char -> Bool
isASCIIAlpha c = 
    let code = toCode c
    in Predundant.between (65, 90) code || Predundant.between (97, 122) code

-- Returns a bool indicating if the character is ASCII and
-- is one of the following: alphabetical, digit, or underscore "_".
isASCIIAlphaNumUnderscore :: Char -> Bool
isASCIIAlphaNumUnderscore c = 
    isASCIIAlpha c || isDigit c || c == '_'
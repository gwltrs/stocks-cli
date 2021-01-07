module IndyComposing (CompositionError(..), composeIndy) where

import Data.List (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Category ((>>>))

import Types
import Predundant

-- Provides granular debugging for the indy script writer.
data CompositionError = 
    FuncNameNotFound String | 
    FuncWithInvalidArgs String
        deriving (Eq, Show)

-- Links the parsed indy script to implemented indy functions.
composeIndy :: [IndyFunc] -> IndyParsed -> Either CompositionError Indicator
composeIndy funcs (IndyParsed parsedName parsedArgs) =
    case findIndyFuncByName parsedName funcs of
        Nothing -> Left (FuncNameNotFound parsedName)
        Just foundIndyFunc -> (parsedArgs <&> mapParsedArg funcs & sequenceA)
            >>= (call foundIndyFunc >>> toEither (FuncWithInvalidArgs parsedName))

-- Internal helper.
-- This is the recursive function that does that actual IndyParsed traversal.
mapParsedArg :: 
    [IndyFunc] 
    -> Either IndyParsed Double
    -> Either CompositionError (Either Indicator Double)
mapParsedArg _ (Right dbl) = Right (Right dbl)
mapParsedArg funcs (Left (IndyParsed ipName ipArgs)) =
    ((findIndyFuncByName ipName funcs
        & toEither (FuncNameNotFound ipName)
        <&> call)
        <*> (ipArgs <&> mapParsedArg funcs & sequenceA))
        >>= toEither (FuncWithInvalidArgs ipName)
        <&> Left

-- Tries to find an indy function with the given name in the list.
findIndyFuncByName :: String -> [IndyFunc] -> Maybe IndyFunc
findIndyFuncByName name indyFuncs =
    find (\iFunc -> indyFuncName iFunc == name) indyFuncs

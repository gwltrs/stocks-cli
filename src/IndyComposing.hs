module IndyComposing where

import Types

data CompositionResult = 
    ComposedIndicator Indicator | 
    FuncNameNotFound String | 
    FuncWithInvalidArgs String

-- Links the parsed indy script to implemented indy functions.
composeIndy :: [IndyFunc] -> IndyParsed -> CompositionResult
composeIndy indyFuncs indyParsed =
    undefined
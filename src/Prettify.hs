module Prettify where

import Data.Function ((&))

import Types (CLICommand(name, description), Stock)

-- User-visible description of the command.
prettifyCmd :: CLICommand -> String
prettifyCmd cmd = "\"" ++ (cmd & name & unwords) ++ "\": " ++ description cmd

-- User-visible description of the stocks.
prettifyStocks :: [Stock] -> String
prettifyStocks stocks = undefined
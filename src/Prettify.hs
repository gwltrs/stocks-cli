module Prettify where

import Data.Function ((&))

import Types (CLICommand(name, description))

-- String that is shown to the user on "help".
prettifyCmd :: CLICommand -> String
prettifyCmd cmd = "\"" ++ (cmd & name & unwords) ++ "\": " ++ description cmd
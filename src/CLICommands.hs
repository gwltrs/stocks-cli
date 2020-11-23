module CLICommands where

import System.Exit (exitSuccess)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Foldable (fold)

import Types

helpName :: [String]
helpName = ["help"]

cliCommands :: [CLICommand]
cliCommands = [
    CLICommand { 
        name = helpName,
        description = "Prints the list of available commands", 
        effect = 
            let prettyText cmd = (cmd & name & unwords) ++ ": " ++ description cmd
            in cliCommands <&> prettyText <&> putStrLn & fold & ($>) },
    CLICommand { 
        name = ["quit"],
        description = "Terminates the application", 
        effect = (exitSuccess $>) },
    CLICommand { 
        name = ["peek"],
        description = "Prints the state", 
        effect = (\s -> (s & show & putStrLn) >> pure s) },
    CLICommand { 
        name = ["add"],
        description = "Increments the state", 
        effect = (\s -> pure (s + 1)) } ]
module CLI where

import Types
import Control.Monad (forever)
import System.IO
import Data.List.Extra (trim, lower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Maybes (firstJusts, orElse)
import System.Exit (exitSuccess)
import Data.Foldable (fold)

runCLI :: IO ()
runCLI = do
    putStrLn ("Technical Analysis CLI")
    putStrLn ("\"help\" for list of commands")
    forever ((putStr ">>> " >> hFlush stdout >> getLine) >>= runCommand)
    
runCommand :: String -> IO ()
runCommand cmdStr =
    let 
        parsedCmd = commands <&> parse <&> ($cmdStr) & firstJusts
        printError = putStrLn "Unrecognized command\n\"help\" for list of commands"
    in do 
        parsedCmd `orElse` printError

commands :: [CLICommand]
commands = [
    CLICommand { 
        description = "\"help\": Prints the list of available commands", 
        parse = (\str -> 
            if (str & trim & lower) == "help" 
            then Just (commands <&> description <&> putStrLn & fold)
            else Nothing) },
    CLICommand { 
        description = "\"quit\": Terminates the application", 
        parse = (\str -> 
            if (str & trim & lower) == "quit" 
            then Just exitSuccess
            else Nothing) } ]
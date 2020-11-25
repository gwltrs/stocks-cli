module CLI where

import System.IO (hFlush, stdout)
import Data.List.Extra (trim, lower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Maybes (firstJusts, orElse)
import Data.Foldable (fold, find)

import Types (CLICommand(name, effect), CLIState(..))
import CLICommands (helpName, cliCommands)

-- Main entry point for the CLI
runCLI :: IO ()
runCLI =
    let
        initialState = CLIState { stocks = [] }
        makeNewState state = do
            putStr ">>> " >> hFlush stdout
            getLine >>= runLine state
    in do
        putStrLn ("Technical Analysis CLI")
        putStrLn ("\"" ++ unwords helpName ++ "\" for list of commands")
        foreverWithState makeNewState initialState

-- Parses the user's input and runs the associated state-modifying, effect-producing command
-- or prints an error message and leaves the state unchanged.
runLine :: CLIState -> String -> IO CLIState
runLine state cmdStr =
    let 
        parsedCmd = firstCommandMatch cliCommands cmdStr <&> effect <&> ($state)
        errorString = "Unrecognized command\n\"" ++ unwords helpName ++ "\" for list of commands"
        printError = putStrLn errorString >> pure state
    in
        parsedCmd `orElse` printError

-- Finds the first command that matches the user's input.
-- Is case insensitve and ignores superfluous whitespace.
firstCommandMatch :: [CLICommand] -> String -> Maybe CLICommand
firstCommandMatch commands userInput =
    let isMatch cmd = (userInput & lower & words) == name cmd
    in find isMatch commands

-- Stateful version of Control.Monad.forever. 
foreverWithState :: (a -> IO a) -> a -> IO ()
foreverWithState makeNew init =
    makeNew init >>= foreverWithState makeNew
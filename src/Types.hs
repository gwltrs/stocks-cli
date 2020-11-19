module Types where

data CLICommand = CLICommand {
    -- This is shown to the user when listing all the commands via "help".
    description :: String,
    -- Tries to parse the user's input and if successful 
    -- returns the effect associated with the command.
    parse :: String -> Maybe (CLIState -> IO CLIState)
}

data CLIState = CLIState {
    charts :: Maybe [String]
}
module Types where

data CLICommand = CLICommand {
    -- Whitespace-delimited text the user enters to executed the command
    -- For example, name = ["download", "data"] means the user types "download data"
    -- Name tokens should be lowercase and not have extraneous whitespace
    name :: [String],
    -- Documentation shown to the user when listing all the commands via "help".
    description :: String,
    -- Performs the IO associated with the command and if necessary updates the state.
    effect :: CLIState -> IO CLIState
}

type CLIState = Int
-- data CLIState = CLIState {
--     stocks :: Maybe [Stock]
-- }

data Stock = Stock {
    symbol :: String,
    days :: [Day]
}

data Day = Day {
    date :: String, -- YYYYMMDD
    open :: Float,
    high :: Float,
    low :: Float,
    close :: Float,
    volume :: Int
}
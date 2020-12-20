module Constants where

-- Name of environment variable holding the API key to eodhistoricaldata.com
eodhdAPIKeyEnvVar :: String
eodhdAPIKeyEnvVar = "EODHD_API_KEY"

-- Amount of delay between each days fetch.
-- Helps prevent requests from reaching limit.
-- In milliseconds.
daysFetchDelay :: Int
daysFetchDelay = 100
module Constants where

-- Name of environment variable holding the API key to eodhistoricaldata.com
eodhdAPIKeyEnvVar :: String
eodhdAPIKeyEnvVar = "EODHD_API_KEY"

-- Stocks from eodhistoricaldata.com are fetched in batches.
-- This saves time but also prevents too-many-request errors.
-- This constant determines the max number of stocks that 
-- can be fetched at a time.
eodhdMaxConcurrentStockFetches :: Int
eodhdMaxConcurrentStockFetches = 8
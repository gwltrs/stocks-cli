module Constants where

-- Name of environment variable holding the API key to eodhistoricaldata.com
eodhdAPIKeyEnvVar :: String
eodhdAPIKeyEnvVar = "EODHD_API_KEY"

-- Stocks are fetched in batches.
-- This saves time but also prevents too-many-request errors.
-- This constant determines the max number of stocks that 
-- can be fetched at a time.
maxConcurrentStockFetches :: Int
maxConcurrentStockFetches = 8
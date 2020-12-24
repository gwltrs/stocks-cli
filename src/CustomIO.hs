module CustomIO where

import Prelude hiding (writeFile, readFile)
import System.IO (FilePath, hFlush, stdout)
import qualified Data.Text.IO as TextIO (writeFile, readFile)
import Data.Text (Text)
import Control.Exception (handle, handleJust, SomeException(..), Exception(..), fromException, displayException)
import Data.Typeable (typeOf)
import Data.Function ((&))
import GHC.IO.Exception (IOErrorType(..))
import Control.Category ((>>>))
import Data.Functor ((<&>))
import Network.Wreq (get, responseBody)
import Control.Lens.Getter ((^.))
import Data.ByteString.Lazy (ByteString)
import qualified System.Environment as SysEnv (lookupEnv)
import Control.Concurrent (threadDelay)
import Data.Either (rights)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.List.NonEmpty as NE (nonEmpty)
import Control.Monad.Except (ExceptT(..), withExceptT, runExceptT)
import Control.Error.Util (failWithM)
import Control.Concurrent.Async (mapConcurrently)
import Data.List.Split (chunksOf)
import Control.Monad (join)

import Predundant
import Railway
import Types
import EODHD
import Constants (maxConcurrentStockFetches)

-- Converts an exception-throwing IO to one that now returns a Left 
-- containing the description of the exception it would have thrown 
-- before conversion. Returns intended value in a Right if operation is successful.
catchIntoExceptT :: IO a -> ExceptT String IO a
catchIntoExceptT unsafeIO = unsafeIO
    <&> Right
    & handle (\e -> (e :: SomeException) & displayException & Left & pure)
    & ExceptT

printAndReturn :: a -> ExceptT String IO a -> IO a
printAndReturn errValue exceptt = 
    exceptt
        & runExceptT
        >>= either (\e -> putStrLn e >> pure errValue) pure

returnError :: String -> ExceptT String IO a
returnError errStr =
    ExceptT $ pure $ Left $ errStr

returnSuccess :: Monad m => a -> ExceptT e m a
returnSuccess = Right >>> pure >>> ExceptT

liftM :: Monad m => m a -> ExceptT e m a
liftM m = m <&> Right & ExceptT

-- Safe version of Data.Text.IO that produces
-- an error string inside a Just value instead
-- of throwing an exception.
writeFile :: FilePath -> Text -> ExceptT String IO ()
writeFile filePath text = 
    TextIO.writeFile filePath text
        & catchIntoExceptT
        & withExceptT ("File write failed\n" ++)

-- Safe version of Data.Text.IO that produces an error
-- string inside a Left value instead of throwing an
-- exception. If successful, returns the file text
-- in a Right value. 
readFile :: FilePath -> ExceptT String IO Text
readFile filePath = 
    TextIO.readFile filePath
        & catchIntoExceptT
        & withExceptT ("File read failed\n" ++)

-- Combined functionalities of Prelude.putStr and Prelude.getLine.
-- Fixes traditional out-of-order rendering issue by flushing buffer.
prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

promptSymbolsDate :: ExceptT String IO YYYYMMDD
promptSymbolsDate = prompt "Get symbols from which date (YYYYMMDD): "
    <&> ymd
    & failWithM "Invalid date"

promptDaysStartingYear :: ExceptT String IO String
promptDaysStartingYear = prompt "Get data starting at year (YYYY): "
    <&> (++ "0101")
    <&> ymd
    & failWithM "Invalid year"
    <&> (str >>> take 4)

getBody :: String -> ExceptT String IO ByteString
getBody url = get url
    <&> (^. responseBody)
    & catchIntoExceptT

-- Just used for getting tests stocks data.
-- Delete later.
getDays :: String -> String -> String -> ExceptT String IO String
getDays apiKey year symbol =
    daysURL apiKey year symbol
        & getBody
        <&> show

-- Attempts to fetch a stock for each given symbol. Discards unvalid days/stocks.
-- Prints out each symbol as it is fetched.
getStocks :: String -> String -> [String] -> IO [Stock]
getStocks apiKey year symbols =
    zip [0..] (take 500 symbols)
        & chunksOf maxConcurrentStockFetches
        <&> mapConcurrently (\t ->
            let url = daysURL apiKey year (snd t)
            in do
                putStrLn (snd t)
                url & getBody <&> (\b -> (snd t, b)) & runExceptT)
        & sequenceA
        <&> join
        <&> rights
        <&> mapMaybe (\t -> t & snd & parseDays >>= NE.nonEmpty >>= (stock (fst t)))

-- SysEnv.lookupEnv lifted into ExceptT with a human-readable error.
lookupEnv :: String -> ExceptT String IO String
lookupEnv envVar = SysEnv.lookupEnv envVar
    & failWithM ("Couldn't find environment variable: " ++ envVar)
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
import Control.Monad.Except (ExceptT(..), withExceptT, runExceptT)
import Control.Error.Util (failWithM)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (join)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV
import Data.Vector.Split (chunksOf)
import Data.Either.Extra (eitherToMaybe)
import Data.List.Extra (trim)

import Predundant
import Types
import EODHD
import Constants (eodhdMaxConcurrentStockFetches)

-- Converts an exception-throwing IO to one that now returns a Left 
-- containing the description of the exception it would have thrown 
-- before conversion. Returns intended value in a Right if operation is successful.
catchIntoExceptT :: IO a -> ExceptT String IO a
catchIntoExceptT unsafeIO = unsafeIO
    <&> Right
    & handle (\e -> pure $! Left $! displayException $ (e :: SomeException))
    & ExceptT

printAndReturn :: a -> ExceptT String IO a -> IO a
printAndReturn errValue exceptt = 
    exceptt
        & runExceptT
        >>= either (\e -> putStrLn e >> pure errValue) pure

returnError :: String -> ExceptT String IO a
returnError errStr =
    ExceptT $! pure $! Left $! errStr

returnSuccess :: Monad m => a -> ExceptT e m a
returnSuccess s = ExceptT $! pure $! Right $! s

liftM :: Monad m => m a -> ExceptT e m a
liftM m = ExceptT $! (m <&> Right)

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
prompt str = putStr str >> hFlush stdout >> getLine <&> trim

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
getStocks :: String -> String -> V.Vector String -> IO (V.Vector Stock)
getStocks apiKey year symbols =
    V.indexed (V.take 100 symbols)
        & chunksOf eodhdMaxConcurrentStockFetches
        & V.fromList
        <&> mapConcurrently (\t ->
            let url = daysURL apiKey year (snd t)
            in do
                putStrLn (snd t)
                url & getBody <&> (\b -> (snd t, b)) & runExceptT)
        & sequenceA
        <&> join
        <&> V.mapMaybe eitherToMaybe
        <&> V.mapMaybe (\t -> t & snd & parseDays >>= NEV.fromVector >>= (stock (fst t)))

-- SysEnv.lookupEnv lifted into ExceptT with a human-readable error.
lookupEnv :: String -> ExceptT String IO String
lookupEnv envVar = SysEnv.lookupEnv envVar
    & failWithM ("Couldn't find environment variable: " ++ envVar)
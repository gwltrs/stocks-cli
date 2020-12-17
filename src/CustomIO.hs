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

import Predundant
import Railway
import Types

type ExceptionType = String

data WriteFileResult = 
    WFSuccess |
    WFError ExceptionType

-- Safe version of Data.Text.IO that produces
-- an error string inside a Just value instead
-- of throwing an exception.
writeFile :: FilePath -> Text -> IO (Either String ())
writeFile filePath text = 
    TextIO.writeFile filePath text
        & toEitherIO
        <<&> ("File write failed\n" ++)

-- Safe version of Data.Text.IO that produces an error
-- string inside a Left value instead of throwing an
-- exception. If successful, returns the file text
-- in a Right value. 
readFile :: FilePath -> IO (Either String Text)
readFile filePath = 
    TextIO.readFile filePath
        & toEitherIO
        <<&> ("File read failed\n" ++)

-- Combined functionalities of Prelude.putStr and Prelude.getLine.
-- Fixes traditional out-of-order rendering issue by flushing buffer.
prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

getBody :: String -> IO (Either String ByteString)
getBody url = get url
    <&> (^. responseBody)
    & toEitherIO

lookupEnv :: String -> IO (Either String String)
lookupEnv envVar = SysEnv.lookupEnv envVar
    ?^| ("Couldn't find environment variable: " ++ envVar)
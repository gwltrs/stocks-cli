module SafeIO (writeFileSafely, readFileSafely, prompt) where

import Prelude hiding (writeFile, readFile)
import System.IO (FilePath, hFlush, stdout)
import Data.Text.IO (writeFile, readFile)
import Data.Text (Text)
import Control.Exception (handle, handleJust, SomeException(..), Exception(..), fromException, displayException)
import Data.Typeable (typeOf)
import Data.Function ((&))
import GHC.IO.Exception (IOErrorType(..))
import Control.Category ((>>>))
import Data.Functor ((<&>))

type ExceptionType = String

data WriteFileResult = 
    WFSuccess |
    WFError ExceptionType

-- Safe version of Data.Text.IO that produces
-- an error string inside a Just value instead
-- of throwing an exception.
writeFileSafely :: FilePath -> Text -> IO (Maybe String)
writeFileSafely filePath text = 
    writeFile filePath text & toSafeMaybeIO

-- Safe version of Data.Text.IO that produces an error
-- string inside a Left value instead of throwing an
-- exception. If successful, returns the file text
-- in a Right value. 
readFileSafely :: FilePath -> IO (Either String Text)
readFileSafely = readFile >>> toSafeEitherIO

-- Combined functionalities of Prelude.putStr and Prelude.getLine.
-- Fixes traditional out-of-order rendering issue by flushing buffer.
prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

-- Converts an IO that would normally return unit to an IO that 
-- now returns a Just containing the description of the exception
-- it would have thrown before conversion.
toSafeMaybeIO :: IO () -> IO (Maybe String)
toSafeMaybeIO unsafeIO = unsafeIO
    >> pure Nothing
    & handle (\e -> (e :: SomeException) & displayException & Just & pure)

-- Converts an IO that would normally return a value to an IO 
-- that now returns a Left containing the description of the
-- exception it would have thrown before conversion. Returns intended
-- value in a Right if operation is successful.
toSafeEitherIO :: IO a -> IO (Either String a)
toSafeEitherIO unsafeIO = unsafeIO
    <&> Right
    & handle (\e -> (e :: SomeException) & displayException & Left & pure)
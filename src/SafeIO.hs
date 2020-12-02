module SafeIO where

import Prelude hiding (writeFile)
import System.IO (FilePath)
import Data.Text.IO (writeFile)
import Data.Text (Text)
import Control.Exception (handle, handleJust, SomeException(..), Exception(..), fromException, displayException)
import Data.Typeable (typeOf)
import Data.Function ((&))
import GHC.IO.Exception (IOErrorType(..))

type ExceptionType = String

data WriteFileResult = 
    WFSuccess |
    WFError ExceptionType

-- Safe version of Data.Text.IO that produces
-- a result type instead of throwing exceptions.
writeFileSafely :: FilePath -> Text -> IO (Maybe String)
writeFileSafely filePath text = 
    writeFile filePath text & toSafeMaybeIO

-- Converts an IO that would normally return unit to an IO that 
-- now returns a Just containing the description of the exception
-- it would have thrown before conversion.
toSafeMaybeIO :: IO () -> IO (Maybe String)
toSafeMaybeIO unsafeIO = unsafeIO
    >> pure Nothing
    & handle (\e -> (e :: SomeException) & displayException & Just & pure)
module Globals where

import Data.IORef
import System.IO.Unsafe

readGlobal (Global g) = readIORef g
writeGlobal (Global g) = writeIORef g

newtype Global a = Global (IORef a)

hadError = Global $ unsafePerformIO $ newIORef False
hadRuntimeError = Global $ unsafePerformIO $ newIORef False

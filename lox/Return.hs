module Return where

import Control.Exception
import Data.Dynamic

data Return = Return Dynamic
instance Exception Return
instance Show Return where
  show = error "Undefined Show instance for Return"

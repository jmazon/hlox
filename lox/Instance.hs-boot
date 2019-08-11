module Instance where

import {-# SOURCE #-} Class

data LoxInstance

newInstance :: LoxClass -> IO LoxInstance

module Instance where

import {-# SOURCE #-} LoxClass

data LoxInstance

newInstance :: LoxClass -> IO LoxInstance

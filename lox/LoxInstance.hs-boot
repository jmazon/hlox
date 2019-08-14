module LoxInstance where

import {-# SOURCE #-} LoxClass

data LoxInstance

newInstance :: LoxClass -> IO LoxInstance

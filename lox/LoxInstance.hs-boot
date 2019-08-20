module LoxInstance (LoxInstance,newInstance) where

import {-# SOURCE #-} LoxClass (LoxClass)

data LoxInstance

newInstance :: LoxClass -> IO LoxInstance

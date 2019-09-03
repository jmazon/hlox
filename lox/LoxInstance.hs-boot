module LoxInstance (LoxInstance,newInstance) where

import Control.Monad.Trans

import {-# SOURCE #-} LoxClass (LoxClass)

data LoxInstance

newInstance :: MonadIO m => LoxClass -> m LoxInstance

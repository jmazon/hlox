module Value where

import {-# SOURCE #-} LoxInstance
import {-# SOURCE #-} LoxCallable

data Value = VNull | VNumber Double | VBool Bool | VString String
           | VCallable MkCallable | VInstance LoxInstance


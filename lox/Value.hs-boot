module Value where

import {-# SOURCE #-} Instance
import {-# SOURCE #-} Callable

data Value = VNull | VNumber Double | VBool Bool | VString String
           | VCallable MkCallable | VInstance LoxInstance


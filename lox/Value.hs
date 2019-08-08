module Value where

data Value = VNull | VNumber Double | VBool Bool | VString String deriving Eq

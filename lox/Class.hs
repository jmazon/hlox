module Class where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Monad

import Callable
import Function
import {-# SOURCE #-} Value
import {-# SOURCE #-} Instance

data LoxClass = LoxClass { className :: String
                         , classMethods :: HashMap String LoxFunction }

instance Callable LoxClass where
  arity c =  do
    case findMethod c "init" of
      Just initializer -> arity initializer
      Nothing -> 0
  call c i arguments = do
    inst <- newInstance c
    let initializer = findMethod c "init"
    maybe (return ()) (flip bind inst >=> \c -> void $ call c i arguments) initializer
    return (VInstance inst)
  toString c = Class.className c

newClass = LoxClass

findMethod :: LoxClass -> String -> Maybe LoxFunction
findMethod c name = H.lookup name (classMethods c)

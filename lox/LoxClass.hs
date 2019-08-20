module LoxClass (LoxClass,className,newClass,findMethod) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Monad
import Control.Applicative
import Data.Unique
import Data.Dynamic

import LoxCallable (LoxCallable,arity,call,toString,callableId)
import LoxFunction (LoxFunction,bind)
import {-# SOURCE #-} LoxInstance (newInstance)

data LoxClass = LoxClass { className :: String
                         , classSuperclass :: Maybe LoxClass
                         , classMethods :: HashMap String LoxFunction
                         , classId :: Unique }

instance LoxCallable LoxClass where
  arity c = case findMethod c "init" of
    Just initializer -> arity initializer
    Nothing -> 0
  call c i arguments = do
    inst <- newInstance c
    let initializer = findMethod c "init"
    maybe (return ()) (flip bind inst >=> \c' -> void $ call c' i arguments) initializer
    return (toDyn inst)
  toString = className
  callableId = classId

newClass :: String -> Maybe LoxClass -> HashMap String LoxFunction -> IO LoxClass
newClass name super methods = LoxClass name super methods <$> newUnique

findMethod :: LoxClass -> String -> Maybe LoxFunction
findMethod c name = H.lookup name (classMethods c) <|>
                    (classSuperclass c >>= \sc -> findMethod sc name)

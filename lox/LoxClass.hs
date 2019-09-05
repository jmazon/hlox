{-# LANGUAGE OverloadedStrings #-}
module LoxClass (LoxClass,className,newClass,findMethod) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Monad
import Control.Applicative ((<|>))
import Data.Unique
import Data.Dynamic
import Data.Text (Text)
import Control.Monad.Trans (MonadIO,liftIO)

import LoxCallable (LoxCallable,arity,call,toString,callableId)
import LoxFunction (LoxFunction,bind)
import {-# SOURCE #-} LoxInstance (newInstance)

data LoxClass = LoxClass { className :: Text
                         , classSuperclass :: Maybe LoxClass
                         , classMethods :: HashMap Text LoxFunction
                         , classId :: Unique }

instance LoxCallable LoxClass where
  arity c = case findMethod c "init" of
    Just initializer -> arity initializer
    Nothing -> 0
  call c arguments = do
    inst <- newInstance c
    let initializer = findMethod c "init"
    mapM_ (bind inst >=> flip call arguments) initializer
    return (toDyn inst)
  toString = className
  callableId = classId

newClass :: MonadIO m => Text -> Maybe LoxClass -> HashMap Text LoxFunction -> m LoxClass
newClass name super methods = LoxClass name super methods <$> liftIO newUnique

findMethod :: LoxClass -> Text -> Maybe LoxFunction
findMethod c name = H.lookup name (classMethods c) <|>
                    (classSuperclass c >>= \sc -> findMethod sc name)

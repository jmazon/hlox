{-# LANGUAGE ExistentialQuantification #-}
module LoxCallable where

class Callable c
data MkCallable = forall c. Callable c => MkCallable c

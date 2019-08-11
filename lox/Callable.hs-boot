{-# LANGUAGE ExistentialQuantification #-}
module Callable where

class Callable c
data MkCallable = forall c. Callable c => MkCallable c

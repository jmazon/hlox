module Util (
    ifM
  , caseM
  , whenM
  , unlessM
  , module Control.Monad.Loops
  ) where

import Control.Monad
import Control.Monad.Loops

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = c >>= \p -> if p then t else e

caseM :: Monad m => [(m Bool,m a)] -> m a -> m a
caseM cs0 def = go cs0 where
  go ((c,b):cs) = do p <- c
                     if p then b else go cs
  go [] = def

whenM :: Monad m => m Bool -> m () -> m ()
whenM c b = do p <- c
               when p b

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c b = do p <- c
                 unless p b

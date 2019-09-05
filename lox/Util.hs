module Util (
    ifM
  , caseM
  , whenM
  , whenM'
  , unlessM
  , module Control.Monad.Loops
  ) where

import Control.Monad
import Control.Monad.Loops

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = c >>= \p -> if p then t else e

caseM :: Monad m => [m (Maybe a)] -> m a -> m a
caseM clauses a = go clauses where
  go (c:cs) = c >>= maybe (go cs) return
  go [] = a

whenM :: Monad m => m Bool -> m () -> m ()
whenM c b = do p <- c
               when p b

whenM' :: Monad m => m Bool -> m a -> m (Maybe a)
whenM' cond body = ifM cond (Just <$> body) (return Nothing)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c b = do p <- c
                 unless p b

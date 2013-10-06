module Wordnet.Util.Monad where

import Control.Monad

cons' :: (Monad m)=> a -> m [a] -> m [a]
cons' = liftM . (:)


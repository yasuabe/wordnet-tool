module Wordnet.Util.StringUtil where

import Control.Monad.State
import Data.Functor.Identity

takeWord ::State String String
takeWord = do
  line <- get
  let (word, rest) = span (' ' /=) line
  put (tail rest)
  return word


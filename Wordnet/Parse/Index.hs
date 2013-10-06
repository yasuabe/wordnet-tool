module Wordnet.Parse.Index where

import Control.Monad.State

import Wordnet.Model.Index
import Wordnet.Util.StringUtil
import Wordnet.Util.Monad

parseIndex :: State String Index
parseIndex  = do
  lemma     <- takeWord
  pos       <- takeWord
  _         <- takeWord
  p_cnt     <- takeWord >>= return . read
  sequence_ $  replicate p_cnt takeWord
  sense_cnt <- takeWord >>= return . read
  takeWord
  parseOffset pos sense_cnt >>= return . (Index lemma)
  where
      parseOffset _   0         = return []
      parseOffset pos sense_cnt = do
        offset <- takeWord
        cons' (pos ++ offset) (parseOffset pos (sense_cnt - 1))

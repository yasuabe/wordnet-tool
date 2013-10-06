module Wordnet.MongoDB.Frame where

import Data.Maybe
import Database.MongoDB

import qualified Wordnet.Model.Frame as F

fromBson :: Document -> F.Frame
fromBson d = F.Frame
  { F.index = index
  , F.frame = frame
  } where
    index = fromJust $ d !? "_id"
    frame = fromJust $ d !? "frame"


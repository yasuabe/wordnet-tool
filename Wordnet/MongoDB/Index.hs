{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Wordnet.MongoDB.Index where

import Data.Maybe
import Database.MongoDB

import qualified Wordnet.Model.Index as I

fromBson :: Document -> I.Index
fromBson d = I.Index
  { I.lemma = lemma
  , I.synsetIds = synsetIds 
  } where
    lemma = fromJust $ d !? "_id"
    synsetIds = toSynsetIds $ fromJust $ d !? "synsetIds"
    toSynsetIds :: [Value] -> [String]
    toSynsetIds values = map (fromJust.cast') values


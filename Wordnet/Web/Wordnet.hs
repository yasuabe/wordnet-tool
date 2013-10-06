{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Wordnet.Web.Wordnet where

import Database.MongoDB hiding (lookup)
import Yesod

data Wordnet = Wordnet { pipe :: Pipe }

mkYesodData "Wordnet" $(parseRoutesFile "config/routes")

instance Yesod Wordnet

findSynset synsetId = do
  p <- getYesod >>= return.pipe
  Right m <- access p master "wordnet" $ findSynset'
  return m
  where
    findSynset' = findOne (select ["_id" =: synsetId] "synset")

findLemma lemma = do
  p <- getYesod >>= return.pipe
  access p master "wordnet" $ findLemma' 
  where
    findLemma' = findOne (select ["_id" =: lemma] "index")

findFrame index = do
  p <- getYesod >>= return.pipe
  Right f <- access p master "wordnet" $ findFrame'
  return f
  where
    findFrame' = findOne (select ["_id" =: index] "frame")

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Wordnet.Web.Result where

import Data.Maybe
import Data.List hiding (find)
import Data.Text (Text, unpack)

import Yesod hiding (count)
import Database.MongoDB hiding (lookup)
import qualified Wordnet.Model.Index as I
import qualified Wordnet.Model.Synset as S
import qualified Wordnet.MongoDB.Synset as MS
import qualified Wordnet.MongoDB.Index as MI
import Wordnet.Web.Wordnet hiding (findLemma)
import Wordnet.Web.ModelUtil
import Wordnet.Web.Common

getResultR :: Handler Html
getResultR = do
  query    <- lookupGetParam "q" >>= return.unpack.fromJust
  skip     <- lookupGetParam "s" >>= return.read.unpack.fromJust
  category <- lookupGetParam "c" 
  (result, rest) <- findLemma query skip category
  let indices    = map MI.fromBson result
  defaultLayout [whamlet|
    ^{header}
    <ul>
    $forall index <- indices
      <li>
        <a href=#{(++) "/index/" $ extractLemma $ I.lemma index}>#{showLemma $ I.lemma index}
    $if skip > 0
      <a href=#{prev query skip category}>prev <<
    <span style="margin-left:5pt;margin-right:5pt;">#{skip + 1} - #{skip + 10}
    $if rest > 0
      <a href=#{next query skip category}>>> next
  |]
  where
    prev query skip cat = createLink query cat $ max (skip - 10) 0
    next query skip cat = createLink query cat $ skip + 10
    createLink query cat skip = case cat of
      Just c  -> base ++ "&c=" ++ (unpack c)
      Nothing -> base
      where base = "/result?q=" ++ query ++ "&s=" ++ (show skip)

findLemma queryString skip cat = do
  Right result <- run findLemma'
  Right rest   <- run $ count query2
  return (result, rest)
  where
    skip' = fromIntegral skip
    findLemma' = find query1 >>= rest
    q = "^" ++ (queryString::String) ++ "$"
    query0 = select (cond ["_id" =: ["$regex" =: q, "$options" =: "i"]]) "index"
    query1 = query0 { skip = skip', limit = 10 }
    query2 = query0 { skip = skip' + 10}
    cond base = case cat of
      Just c -> ("synsetIds" =: ["$regex" =: '^' : unpack c]) : base
      Nothing -> base
    run act = do { getYesod >>= (\y->access (pipe y) master "wordnet" act) }

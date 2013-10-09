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
  query'    <- lookupGetParam "q" >>= return.unpack.fromJust
  let query = query'::String
  skip      <- lookupGetParam "s" >>= return.unpack.fromJust
  cat       <- lookupGetParam "c" 
  let skip'      =  read skip
  (result, rest) <- findLemma query skip' cat
  let indices    = map MI.fromBson result
  defaultLayout [whamlet|
    ^{header}
    <ul>
    $forall index <- indices
      <li>
        <a href=#{(++) "/index/" $ extractLemma $ I.lemma index}>#{showLemma $ I.lemma index}
    $if skip' > 0
      <a href=#{prev query skip' cat}>prev <<
    <span style="margin-left:5pt;margin-right:5pt;">#{skip' + 1} - #{skip' + 10}
    $if rest > 0
      <a href=#{next query skip' cat}>>> next
  |]
  where
    prev query skip cat = f cat $ "/result?q=" ++ query ++ "&s=" ++ (show (max (skip - 10) 0))
    next query skip cat = f cat $ "/result?q=" ++ query ++ "&s=" ++ (show (skip + 10))
    f cat base = case cat of
      Just c -> base ++ "&c=" ++ (unpack c)
      Nothing -> base

findLemma queryString skip cat = do
  p <- getYesod >>= return.pipe
  Right result <- access p master "wordnet" $ findLemma'
  Right rest   <- access p master "wordnet" $ count query2
  return (result, rest)
  where
    skip' = fromIntegral skip
    findLemma' = find query1 >>= rest
    q = queryString::String
    query0 = select (cond ["_id" =: ["$regex" =: "^" ++ q ++ "$", "$options" =: "i"]]) "index"
    query1 = query0 { skip = skip', limit = 10 }
    query2 = query0 { skip = skip' + 10}
    cond base = case cat of
      Just c -> ("synsetIds" =: ["$regex" =: '^' : unpack c]) : base
      Nothing -> base

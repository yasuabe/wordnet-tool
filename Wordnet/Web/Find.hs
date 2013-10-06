{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Wordnet.Web.Find where

import Data.Maybe
import Data.List hiding (find)
import Data.Text (unpack)

import Yesod hiding (count)
import Database.MongoDB hiding (lookup)
import qualified Wordnet.Model.Index as I
import qualified Wordnet.Model.Synset as S
import qualified Wordnet.MongoDB.Synset as MS
import qualified Wordnet.MongoDB.Index as MI
import Wordnet.Web.Wordnet hiding (findLemma)
import Wordnet.Web.ModelUtil
import Wordnet.Web.Common

getHomeR :: Handler Html
getHomeR = do defaultLayout [whamlet|
    <form method=get action="find">
      <input type="text"   name="q">
      <input type="hidden" name="s" value="0">
  |]

getFindR :: Handler Html
getFindR = do
  query     <- lookupGetParam "q" >>= return.unpack.fromJust
  skip      <- lookupGetParam "s" >>= return.unpack.fromJust
  let skip'      =  read skip
  (result, rest) <- findLemma query skip'
  let indices    = map MI.fromBson result
  defaultLayout [whamlet|
    ^{header}
    <ul>
    $forall index <- indices
      <li>
        <a href=#{(++) "/index/" $ extractLemma $ I.lemma index}>#{showLemma $ I.lemma index}
    $if skip' > 0
      <a href=#{prev query skip'}>prev <<
    <span style="margin-left:5pt;margin-right:5pt;">#{skip' + 1} - #{skip' + 10}
    $if rest > 0
      <a href=#{next query skip'}>>> next
  |]
  where
    prev query skip = "/find?q=" ++ query ++ "&s=" ++ (show (max (skip - 10) 0))
    next query skip = "/find?q=" ++ query ++ "&s=" ++ (show (skip + 10))

findLemma queryString skip = do
  p <- getYesod >>= return.pipe
  Right result <- access p master "wordnet" $ findLemma'
  Right rest   <- access p master "wordnet" $ count query2
  return (result, rest)
  where
    skip' = fromIntegral skip
    findLemma' = find query1 >>= rest
    query0 = (select ["_id" =: ["$regex" =: queryString, "$options" =: "i"]] "index")
    query1 = query0 { skip = skip', limit = 10 }
    query2 = query0 { skip = skip' + 10}


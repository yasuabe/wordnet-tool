{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordnet.Web.Index where

import Data.Maybe
import Yesod
import Data.List (groupBy)
import Data.Function

import qualified Wordnet.Model.Index as I
import qualified Wordnet.Model.Synset as S

import qualified Wordnet.MongoDB.Synset as MS
import qualified Wordnet.MongoDB.Index as MI

import Wordnet.Web.Wordnet
import Wordnet.Web.ModelUtil
import Wordnet.Web.Common

getIndexR lemma = do
  Right matched <- findLemma lemma
  if isJust matched
    then do
      let synsetIds =  I.synsetIds $ MI.fromBson $ fromJust matched
      synsets       <- mapM findSynset synsetIds
      let synsets'  =  map (MS.fromBson.fromJust) synsets
      let synsetGroups = groupSynset synsets'
      defaultLayout [whamlet|
        ^{header}
        <h1>#{showLemma lemma}
        $forall group <- synsetGroups
          $with synset1 <- head group
            <h2>#{showCategory $ S._id synset1}
            <dl>
              $forall synset <- group
                $with synsetId <- S._id synset 
                  <dt>
                    <a href=#{"/synset/" ++ synsetId}> [#{synsetId}]
                  <dd>
                    $forall d <- (S.definitions (S.gloss synset))
                      #{d}<br>
      |]
    else
      defaultLayout noMatch

groupSynset synsets = groupBy (on (==) (head.S._id)) synsets

showCategory :: String -> String  
showCategory synsetId =
  case head synsetId of
    'n' -> "noun"
    'v' -> "verb"
    'a' -> "adjective"
    'r' -> "adverb"

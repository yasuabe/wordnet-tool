{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordnet.Web.Synset where

import Data.Maybe
import Yesod
import Data.List (groupBy, sortBy)
import Data.Function

import qualified Wordnet.Model.Index as I
import qualified Wordnet.Model.Synset as S
import qualified Wordnet.Model.Frame as F

import qualified Wordnet.MongoDB.Synset as MS
import qualified Wordnet.MongoDB.Index as MI
import qualified Wordnet.MongoDB.Frame as MF

import Wordnet.Web.Wordnet
import Wordnet.Web.Common

getSynsetR :: String -> Handler Html
getSynsetR synsetId = do
  matched <- findSynset synsetId
  if isJust matched
    then do
      let synset = MS.fromBson $ fromJust matched
      let synsetId = S._id synset
      let pos = head synsetId
      let words = S.words synset
      let firstWord = head words
      let definitions = S.definitions $ S.gloss synset
      let examples = S.examples $ S.gloss synset
      let groupedPointers = groupBy (\a b->S.symbol a == S.symbol b) $ sortPointers pos $ S.pointers synset
      pointedSynsets <- h $ S.pointers synset
      let maybeFrames = S.frames synset
      frameD <- mapM findFrame $ frameNums maybeFrames
      let frameDocs = map (\f -> (F.index f, F.frame f)) $ map (MF.fromBson.fromJust) frameD
      defaultLayout [whamlet|
        ^{header}
        <h1>
          $with w1 <- S.word firstWord
            <a href=#{"/index/" ++ (extractLemma w1)}>#{showLemma w1}
            $forall word <- (tail words)
              ,
              $with w <- S.word word
                <a href=#{"/index/" ++ (extractLemma w)}>#{showLemma w}
        <h2>#{showLexNum $ S.lex_filenum synset}
        <p>
          $if (not $ null definitions)
            <dl>
              <dt>DEFINITION
              <dd>
                <ul>
                  $forall definition <- definitions
                    <li>#{definition}
          $if (isJust maybeFrames)
            <dl>
              <dt>FRAME
              <dd>
                $forall frame <- fromJust maybeFrames
                  $with f <- fromJust $ lookup (S.f_num frame) frameDocs
                    $if (0 == (S.w_num frame))
                      #{f}<br>
                    $else
                      #{frameWithWord f frame words}<br>
          $if (not $ null examples)
            <dl>
              <dt>EXAMPLE
                <dd>
                  <ul>
                    $forall example <- examples
                      <li>#{example}
          $if (not $ null groupedPointers)
            $forall group <- groupedPointers
              <dl>
                <dt>#{(flip symbolToString (head synsetId)) $ S.symbol $ head group}
                <dd>
                  $forall pointer <- group
                    <a href=#{S.synset_id pointer} style="margin-right:5pt;">[#{S.synset_id pointer}]
                    $if hasSourceTarget pointer
                      (#{g pointer words} -> &nbsp;
                      $with dest <- g1 pointer pointedSynsets
                        <a href=#{"/index/" ++ (extractLemma dest)}>#{showLemma dest}
                      )
                    $else
                      $with lemmas <- (map S.word $ S.words $ fromJust $ lookup (S.synset_id pointer) pointedSynsets)
                        <a href=#{"/index/" ++ (extractLemma $ head lemmas)}>#{showLemma $head lemmas}
                        $forall lemma <- tail lemmas
                          , &nbsp;
                          <a href=#{"/index/" ++ (extractLemma lemma)}>#{showLemma lemma}
                    <br>
      |]
    else defaultLayout [whamlet|<p>no match|]

h pointers = do
  synsets <- mapM findSynset synsetIds
  return $ map (\ss -> (S._id ss, ss)) $ map (MS.fromBson.fromJust) synsets
  where
    synsetIds = map S.synset_id pointers

frameNums (Just frames) = map S.f_num frames
frameNums Nothing     = []

showLemma "" = ""
showLemma ('_':t) = ' ':showLemma t
showLemma (h:t)   = h: showLemma t

extractLemma = takeWhile (/='(')

g pointer words = f source words where
  source  = S.source pointer

f source sourceWords = (S.word $ sourceWords!!(source - 1))

g1 pointer targets = f1 target targetWords where
  targetSynset = fromJust $ lookup (S.synset_id pointer) targets
  target  = S.target pointer
  targetWords = S.words targetSynset

f1 target targetWords = S.word $ targetWords!!(target - 1)


frameWithWord f frame words = f ++ " (" ++ (S.word$ words!!(S.w_num frame)) ++ ")"

sortPointers pos pointers = sortBy (on compare ((symbolToOrder pos).S.symbol)) pointers
symbolToString :: String -> Char -> String
symbolToString symbol pos = fst $ parseSymbol symbol pos

symbolToOrder :: Char -> String -> Int
symbolToOrder pos symbol = snd $ parseSymbol symbol pos

parseSymbol "!"  _   = ("Antonym", 0)
parseSymbol "#m" _   = ("Member holonym", 1)
parseSymbol "#p" _   = ("Part holonym", 2)
parseSymbol "#s" _   = ("Substance holonym", 3)
parseSymbol "$"  _   = ("Verb Group", 4)
parseSymbol "%m" _   = ("Member meronym", 5)
parseSymbol "%p" _   = ("Part meronym", 6)
parseSymbol "%s" _   = ("Substance meronym", 7)
parseSymbol "*"  _   = ("Entailment", 8)
parseSymbol "+"  _   = ("Derivationally related form", 9)
parseSymbol "c"  _   = ("Member of this domain - TOPIC", 10)
parseSymbol "r"  _   = ("Member of this domain - REGION", 11)
parseSymbol "u"  _   = ("Member of this domain - USAGE", 12)
parseSymbol ";c" _   = ("Domain of synset - TOPIC", 13)
parseSymbol ";r" _   = ("Domain of synset - REGION", 14)
parseSymbol ";u" _   = ("Domain of synset - USAGE", 15)
parseSymbol "="  _   = ("Attribute", 16)
parseSymbol ">"  _   = ("Cause", 17)
parseSymbol "@"  _   = ("Hypernym", 18)
parseSymbol "@i" _   = ("Instance Hypernym", 19)
parseSymbol "~"  _   = ("Hyponym", 21)
parseSymbol "~i" _   = ("Instance Hyponym", 22)
parseSymbol "&"  _   = ("Similar to", 23)
parseSymbol "<"  _   = ("Participle of verb", 24)
parseSymbol "\\" 'a' = ("Pertainym (pertains to noun)", 25)
parseSymbol "\\" 'r' = ("Derived from adjective", 26)
parseSymbol "^"  _   = ("Also see", 120)

hasSourceTarget pointer = 0 /= S.source pointer

showLexNum :: String -> String
showLexNum "00" =  "adj.all"
showLexNum "01" =  "adj.pert"
showLexNum "02" =  "adv.all"
showLexNum "03" =  "noun.Tops"
showLexNum "04" =  "noun.act"
showLexNum "05" =  "noun.animal"
showLexNum "06" =  "noun.artifact"
showLexNum "07" =  "noun.attribute"
showLexNum "08" =  "noun.body"
showLexNum "09" =  "noun.cognition"
showLexNum "10" =  "noun.communication"
showLexNum "11" =  "noun.event"
showLexNum "12" =  "noun.feeling"
showLexNum "13" =  "noun.food"
showLexNum "14" =  "noun.group"
showLexNum "15" =  "noun.location"
showLexNum "16" =  "noun.motive"
showLexNum "17" =  "noun.object"
showLexNum "18" =  "noun.person"
showLexNum "19" =  "noun.phenomenon"
showLexNum "20" =  "noun.plant"
showLexNum "21" =  "noun.possession"
showLexNum "22" =  "noun.process"
showLexNum "23" =  "noun.quantity"
showLexNum "24" =  "noun.relation"
showLexNum "25" =  "noun.shape"
showLexNum "26" =  "noun.state"
showLexNum "27" =  "noun.substance"
showLexNum "28" =  "noun.time"
showLexNum "29" =  "verb.body"
showLexNum "30" =  "verb.change"
showLexNum "31" =  "verb.cognition"
showLexNum "32" =  "verb.communication"
showLexNum "33" =  "verb.competition"
showLexNum "34" =  "verb.consumption"
showLexNum "35" =  "verb.contact"
showLexNum "36" =  "verb.creation"
showLexNum "37" =  "verb.emotion"
showLexNum "38" =  "verb.motion"
showLexNum "39" =  "verb.perception"
showLexNum "40" =  "verb.possession"
showLexNum "41" =  "verb.social"
showLexNum "42" =  "verb.stative"
showLexNum "43" =  "verb.weather"
showLexNum "44" =  "adj.ppl"


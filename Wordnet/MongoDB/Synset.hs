{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Wordnet.MongoDB.Synset where

import Data.Maybe
import Database.MongoDB

import qualified Wordnet.Model.Synset as S

fromBson :: Document -> S.Synset
fromBson d = S.Synset
  { S._id = _id 
  , S.lex_filenum = lexFileNum
  , S.words       = words
  , S.pointers    = pointers
  , S.frames      = frames
  , S.gloss       = gloss
  } where
    _id = fromJust $ d !? "_id"
    lexFileNum = fromJust $ d !? "lex_filenum"
    gloss = S.Gloss
      { S.definitions = definitions
      , S.examples    = examples
      } where
      gloss' = fromJust $ d !? "gloss"
      definitions = toStrings $ fromJust $ gloss' !? "definitions"
      examples    = toStrings $ fromJust $ gloss' !? "examples"
      toStrings :: [Value] -> [String]
      toStrings values = map (fromJust.cast') values
    words = map toWord words'
      where
        words' :: [Document]
        words' = map (fromJust.cast') $ fromJust $ d !? "words"
        toWord doc = S.Word
          { S.word   = fromJust $ doc !? "word"
          , S.lex_id = fromJust $ doc !? "lex_id"
          }
    pointers = map toPointer pointers' where
      pointers' = map (fromJust.cast') $ fromJust $ d !? "pointers"
      toPointer doc = S.Pointer
        { S.symbol = fromJust $ doc !? "symbol"
        , S.synset_id = fromJust $ doc !? "synset_id"
        , S.source = fromJust $ doc !? "source"
        , S.target = fromJust $ doc !? "target"
        }
    frames = nothingIfEmpty $ map toFrames frames' where
      nothingIfEmpty xs = if null xs then Nothing else Just xs
      frames' = map (fromJust.cast') $ fromJust $ d !? "frames"
      toFrames doc = S.Frame
        { S.f_num = fromJust $ doc !? "f_num"
        , S.w_num = fromJust $ doc !? "w_num"
        }
 

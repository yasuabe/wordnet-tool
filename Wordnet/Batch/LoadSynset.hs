{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Wordnet.Batch.LoadSynset where

import Data.Char (isDigit)
import Control.Monad.IO.Class
import Control.Monad.State

import Database.MongoDB

import Wordnet.Util.StringUtil
import Wordnet.Model.Synset as S
import Wordnet.Parse.Synset

path = "/usr/share/wordnet-3.0/dict/data." 

main = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    e <- access pipe master "wordnet" run
    close pipe

run = do
  delete (select [] "synset")
  forM_ files loadSynsetFile
  where
    files = map (path ++) ["noun", "verb", "adj", "adv"]

loadSynsetFile fileName = do
  synsets <- liftIO (loadSynset fileName)
  mapM_ insertSynset synsets
  where
    insertSynset s = insert "synset" $ synsetToFields s

loadSynset fileName = do
  contents        <- readFile fileName
  let synsetLines =  filter isDataLine $ lines contents
  return $ map (evalState parseSynset) synsetLines

isDataLine :: String -> Bool
isDataLine line = length line > 0 && isDigit (line!!0)

synsetToFields s =
      [ "_id"         =: S._id         s
      , "lex_filenum" =: S.lex_filenum s
      , "words"       =: (words    $ S.words    s)
      , "pointers"    =: (pointers $ S.pointers s) 
      , "frames"      =: (frames   $ S.frames   s)
      , "gloss"       =: (gloss    $ S.gloss    s)
      ]
  where
    words []     = []
    words (w:ws) =
      [ "word"   =: (S.word w)
      , "lex_id" =: (S.lex_id w)
      ] : words ws
    pointers []     = []
    pointers (p:ps) =
      [ "symbol"    =: (S.symbol    p)
      , "synset_id" =: (S.synset_id p)
      , "source"    =: (S.source    p)
      , "target"    =: (S.target    p)
      ] : pointers ps
    frames Nothing       = []
    frames (Just fs) = frames' fs where
      frames' [] = []
      frames' (f:fs) =
        [ "f_num" =: (S.f_num f)
        , "w_num" =: (S.w_num f)
        ] : frames' fs
    gloss g =
      [ "definitions" =: (S.definitions g)
      , "examples"    =: (S.examples    g)
      ]

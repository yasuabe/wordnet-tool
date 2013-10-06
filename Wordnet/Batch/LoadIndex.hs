{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Wordnet.Batch.LoadIndex where

import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.State

import Database.MongoDB

import Wordnet.Model.Index as I
import Wordnet.Parse.Index

path  = "/usr/share/wordnet-3.0/dict/index."
files = map (path ++) ["noun", "verb", "adj", "adv"]

main = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    e <- access pipe master "wordnet" run
    close pipe

run = do
  delete (select [] "index")
  ls <- liftIO (readLines files)
  let indices =  M.elems $ execState (mapM extractIndices ls) M.empty
  mapM_ ((insert "index") . indexToFields) indices 

readLines files = foldM appendLines [] files 
  where
    appendLines lines filePath =
      readIndexLines filePath >>= (return . (lines ++))
    readIndexLines fileName =
      readFile fileName >>= return . (filter isDataLine) . lines

extractIndices line = do
  let index = evalState parseIndex line
  let lemma = I.lemma index
  cache <- get
  let index' = mergeIfNeeded index $ M.lookup lemma cache
  put   $  M.insert lemma index' cache
  return index'
  where
    mergeIfNeeded index (Just i) = I.Index 
      { lemma     = I.lemma index
      , synsetIds = (I.synsetIds i ++ I.synsetIds index)
      }
    mergeIfNeeded  index Nothing  = index

isDataLine :: String -> Bool
isDataLine line = length line > 0 && (not.isSpace.head) line

indexToFields s =
      [ "_id"       =: I.lemma     s
      , "synsetIds" =: I.synsetIds s
      ]

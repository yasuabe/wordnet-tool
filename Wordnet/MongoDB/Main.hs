{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Wordnet.MongoDB.Main where

import Control.Monad.IO.Class
import Database.MongoDB
import Wordnet.Util.StringUtil
import Wordnet.Model.Frame as F

loadFrames = do
  frameLines <- readFile "/usr/share/wordnet-3.0/dict/frames.vrb" >>= return.lines
  return $ map wnLineToFrame frameLines

wnLineToFrame wnLine = Frame index frame where
  (index_, frame)    = splitAt 3 wnLine
  index              = read index_


main = do 
    pipe <- runIOE $ connect (host "127.0.0.1")
    e <- access pipe master "wordnet" run
    close pipe

run = do
  frames <- liftIO loadFrames
  mapM_ insertFrame frames
  where
    insertFrame f = insert "frame" ["_id" =: F.index f, "frame" =: F.frame f]

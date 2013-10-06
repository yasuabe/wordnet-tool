module Wordnet.Parse.Synset where

import Numeric
import Data.Maybe
import Data.Char (isSpace)
import Control.Monad.State
import Data.Text (splitOn, pack, unpack)

import Wordnet.Util.StringUtil
import Wordnet.Util.Monad
import Wordnet.Model.Synset

parseSynset :: State String Synset
parseSynset = do
  offset       <- takeWord
  lex_filenum  <- takeWord
  ss_type      <- takeWord
  words        <- parseWords
  pointers     <- parsePointer
  frames       <- parseFrame
  gloss        <- get >>= return . splitGloss
  let synsetId =  makeSynsetId ss_type offset
  return $ Synset synsetId lex_filenum words pointers frames gloss

parseWords = do
  count <- liftM hex takeWord
  takeWords count where
    takeWords 0 = return []
    takeWords c =  do
      word      <- takeWord
      lexId     <- liftM hex takeWord
      cons' (Word word lexId) (takeWords (c - 1))

parsePointer = do
  w  <- takeWord
  takePointer ((read w)::Int) where
    takePointer 0 = return []
    takePointer c = do
      symbol        <- takeWord
      offset        <- takeWord
      pos           <- takeWord
      sourceTarget  <- takeWord
      let synset_id =  makeSynsetId pos offset 
      let source    =  hex $ take 2 sourceTarget
      let target    =  hex $ drop 2 sourceTarget
      cons' (Pointer symbol synset_id source target) (takePointer (c - 1))

makeSynsetId "s" offset = "a" ++ offset
makeSynsetId pos offset = pos ++ offset

parseFrame = do
  line <- get
  if head line == '|'
    then return Nothing
    else takeWord >>= takeFrame .read >>= return .Just  where
      takeFrame 0 = return []
      takeFrame c = do
        takeWord
        frame     <- takeWord >>= return.read
        word      <- takeHex
        cons' (Frame frame word) (takeFrame (c - 1))

splitGloss s = Gloss d e where
  (d, e) = toGloss $ reverse $ g s
g s = filter (not.null) 
    $ map (dropSpace.reverse.dropSpace) 
    $ f s False [[]]
  where
  dropSpace = dropWhile (==' ')
  f []       _       r      = r
  f ('"':cs) False   (l:ls) = f cs True    (('"':l):ls)
  f (';':cs) False   r      = f cs False   ([]     :r)
  f ('"':cs) True    (l:ls) = f cs False   (('"':l):ls)
  f (c  :cs) inQuote (l:ls) = f cs inQuote ((c:l)  :ls)

toGloss [] = ([], [])
toGloss (x:xs)
  | isExample  = (d, (trimBoth trimChar x): e) 
  | otherwise  = ((trimBoth " |" x): d, e)
  where
    (d, e)    = toGloss xs
    isExample = head x == '\"'
    trimChar  = if '\"'==(head $ dropWhile isSpace $ reverse x) then " \"" else " "

trimBoth cs s = trim cs (trim cs s) where
  trim _ []       = []
  trim cs s@(d:t) | elem d cs = trim cs t
                  | otherwise = reverse s

hex hexWord = fst.(!!0) $ readHex hexWord
takeHex = takeWord >>= return.hex



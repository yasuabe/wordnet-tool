module Wordnet.Web.ModelUtil
  ( showLemma
  , extractLemma 
  ) where

showLemma ""      = ""
showLemma ('_':t) = ' ':showLemma t
showLemma (h:t)   = h: showLemma t

extractLemma = takeWhile (/='(')


module Wordnet.Model.Index where

data Index    = Index
  { lemma     :: String
  , synsetIds :: [String] 
  } deriving Show

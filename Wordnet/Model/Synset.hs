module Wordnet.Model.Synset where

data Word       = Word
  { word        :: String
  , lex_id      :: Int
  } deriving Show

data Pointer    = Pointer
  { symbol     :: String
  , synset_id  :: String
  , source     :: Int
  , target     :: Int
  } deriving Show

data Frame      = Frame
  { f_num       :: Int
  , w_num       :: Int
  } deriving Show

type MaybeFrame = Maybe [Frame]

data Gloss = Gloss
  { definitions :: [String]
  , examples    :: [String]
  } deriving Show

data Synset     = Synset 
  { _id         :: String
  , lex_filenum :: String
  , words       :: [Word]
  , pointers    :: [Pointer]
  , frames      :: MaybeFrame
  , gloss       :: Gloss 
  } deriving Show


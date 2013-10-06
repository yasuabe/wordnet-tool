{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Wordnet.Web.Main where

import Data.Maybe
import Data.List
import Yesod
import Database.MongoDB hiding (lookup)
import qualified Wordnet.Model.Index as I
import qualified Wordnet.Model.Synset as S
import qualified Wordnet.MongoDB.Synset as MS
import qualified Wordnet.MongoDB.Index as MI
import Wordnet.Web.Wordnet
import Wordnet.Web.Index
import Wordnet.Web.Synset
import Wordnet.Web.Find

mkYesodDispatch "Wordnet" resourcesWordnet

main :: IO ()
main = do
  pipe <- runIOE $ connect (host "127.0.0.1")
  warp 3000 Wordnet { pipe = pipe }
  close pipe

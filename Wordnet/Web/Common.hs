{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordnet.Web.Common where

import Yesod

header :: WidgetT site IO ()
header = [whamlet|
  <div style="background-color:Aquamarine;">
    <form method=get action="/find">
      <input type="text"   name="q">
      <input type="hidden" name="s" value="0">
|]

noMatch :: WidgetT site IO ()
noMatch = [whamlet|
  ^{header}
  <p>no match
|]


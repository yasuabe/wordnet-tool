{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Wordnet.Web.Common where

import Yesod

header :: WidgetT site IO ()
header = [whamlet|
  <div style="background-color:Aquamarine;">
    <form method=get action="/find">
      <span style="margin-left:10pt;">
        QUICK SEARCH
        <input type="text"   name="q">
      <span style="margin-left:10pt;">
        <a href="/condition">SEARCH
      <input type="hidden" name="s" value="0">
|]

noMatch :: WidgetT site IO ()
noMatch = [whamlet|
  ^{header}
  <p>no match
|]


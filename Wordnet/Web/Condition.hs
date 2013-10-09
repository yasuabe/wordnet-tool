{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Wordnet.Web.Condition where

import Yesod
import Wordnet.Web.Wordnet
import Wordnet.Web.Common

getConditionR :: Handler Html
getConditionR = do
  defaultLayout [whamlet|
    ^{header}
    <form method=get action="/result">
      <input name="q" type="text">
      <div>
        <input type="radio" name="c" value="all" checked>All
        <input type="radio" name="c" value="n">Noun
        <input type="radio" name="c" value="v">Verb
        <input type="radio" name="c" value="a">Adjective
        <input type="radio" name="c" value="r">Adverb
      <input type="submit">
      <input type="hidden" name="s" value="0">
    <ul>
  |]


module Wordnet.Model.SyntacticCategory where

data SyntacticCategory = Noun | Verb | Adjective | Adverb deriving (Show, Eq)

toSyntacticCategory 1 = Noun
toSyntacticCategory 2 = Verb
toSyntacticCategory 3 = Adjective
toSyntacticCategory 4 = Adverb

fromChar 'n' = Noun
fromChar 'v' = Verb
fromChar 'a' = Adjective
fromChar 'r' = Adverb

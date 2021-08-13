module Toml.Parser

import Data.List
import Data.List1
import Extra.List
import Text.Parser
import Text.Token
import Toml.Data
import Toml.Lexer

import public Toml.Tokens

comment : Grammar _ TomlToken True (Maybe (List String, Value))
comment =
  map (const Nothing) (match Comment)

whitespace : Grammar _ TomlToken True (Maybe (List String, Value))
whitespace =
  map (const Nothing) (match Whitespace)

optWhitespace : Grammar _ TomlToken False ()
optWhitespace = ignore $ optional $ match Whitespace

keyword : Grammar _ TomlToken True (List String)
keyword =
  forget <$> sepBy1 (match Dot) (match Keyword <|> match StringLit <|> (map show $ match Number))

heading : Grammar _ TomlToken True (List String)
heading =
  do
    match LeftBracket
    optWhitespace
    key <- keyword
    optWhitespace
    match RightBracket
    pure key


doubleHeading : Grammar _ TomlToken True (List String)
doubleHeading =
  do
    match LeftBracket
    match LeftBracket
    optWhitespace
    key <- keyword
    optWhitespace
    match RightBracket
    match RightBracket
    pure key

doubleHeadingOf : List String -> Grammar _ TomlToken True String
doubleHeadingOf lookup =
  do
    key <- doubleHeading
    the (Grammar _ _ False _) $
      if key == lookup
        then pure "hi"
        else fail "mismatched double heading"

str : Grammar _ TomlToken True Value
str =
  map Str (match StringLit)

num : Grammar _ TomlToken True Value
num =
  map Num (match Number)

optSpacing : Grammar state TomlToken True a -> Grammar state TomlToken True a
optSpacing inner =
  do
    optWhitespace
    res <- inner
    optWhitespace
    pure res

mutual
  list : Grammar state TomlToken True Value
  list =
    do
      ignore $ match LeftBracket
      values <- sepBy (optSpacing $ match Comma) value
      ignore $ match RightBracket
      pure (Lst values)

  value : Grammar _ TomlToken True Value
  value =
    optSpacing $ str <|> num <|> list

kv : Grammar _ TomlToken True (Maybe (List String, Value))
kv =
  do
    key <- keyword
    optWhitespace
    match Equals
    optWhitespace
    v <- value
    pure $ Just (key, v)

mutual
  subtable : Grammar _ TomlToken True (List (List String, Value))
  subtable =
    do
      key <- doubleHeading
      -- Let's read a table and repeat with the same heading any number of times
      tomls <- sepBy1 (doubleHeadingOf key) (simpleToml key)
      let val = ArrTab $ forget tomls
      pure [(key, val)]

  emptyHeading : Grammar _ TomlToken True (List (List String, Value))
  emptyHeading =
    do
      ignore heading
      pure []

  kvs : Grammar _ TomlToken True (List (List String, Value))
  kvs =
    do
      key <- option [] heading
      inner <- some (
            kv
        <|> comment
        <|> whitespace)
      pure $ map (\(k, v) => (key ++ k, v)) (mapMaybe id $ forget inner)

  simpleToml : List String -> Grammar _ TomlToken False Toml
  simpleToml pre =
    do
      res <- map concat (many (kvs <|> emptyHeading))
      pure $ map (mapFst $ dropPrefix pre) res

  toml : Grammar _ TomlToken False Toml
  toml =
    map concat (many (subtable <|> kvs <|> emptyHeading))

export
parseTomlToks : List $ WithBounds TomlToken -> Maybe Toml
parseTomlToks toks = case parse toml toks of
                      Right (j, []) => Just j
                      _ => Nothing

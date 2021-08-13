module Markdown

import Markdown.Lexer
import Markdown.Parser

import Text.Bounded

import public Markdown.Data

%default covering

||| Parse a Markdown string
export
parse : String -> Maybe Markdown
parse x = parseMarkdown !(lexMarkdown x)

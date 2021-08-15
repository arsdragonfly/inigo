module Markdown.Lexer

import Data.List1

import Text.Lexer
import Text.Token
import Text.Bounded

import Markdown.String
import public Markdown.Tokens

%default total

private
markdownTokenMap : TokenMap MarkdownToken
markdownTokenMap = toTokenMap $
  [ (codeFence, MdCodeBlock)
  , (pre, MdPre)
  , (headingSym, HeadingSym)
  , (italicsSym, ItalicsSym)
  , (boldSym, BoldSym)
  , (imageSym, ImageSym)
  , (link, MdLink)
  , (htmlCloseTag, HtmlCloseTag)
  , (htmlOpenTag, HtmlOpenTag)
  , (newLine, NewLine)
  , (text, MdText)
  ]

||| Combine consecutive `MdText` nodes into one
combineText : List (WithBounds MarkdownToken) -> List (WithBounds MarkdownToken)
combineText [] = []
combineText (el :: rest) =
  let
    init = (List1.singleton el, el)
  in
    forget $ reverse $ fst $ (foldl accumulate init rest)
  where
    accumulate : (List1 $ WithBounds MarkdownToken, WithBounds MarkdownToken) -> WithBounds MarkdownToken -> (List1 $ WithBounds MarkdownToken, WithBounds MarkdownToken)
    accumulate (acc0 ::: acc1, last) el =
      case (last, el) of
        ( (MkBounded (Tok MdText a) isIrr (MkBounds startLine startCol _ _)),
          (MkBounded (Tok MdText b) _     (MkBounds _ _ endLine endCol)))
          => let
               combined = (MkBounded (Tok MdText (a ++ b)) isIrr (MkBounds startLine startCol endLine endCol))
             in (combined ::: acc1, combined)
        _ =>
          (el ::: acc0 :: acc1, el)

public export
lexMarkdown : String -> Maybe (List $ WithBounds MarkdownToken)
lexMarkdown str
  = case lex markdownTokenMap str of
         (tokens, _, _, "") => Just $ combineText $ tokens
         _ => Nothing

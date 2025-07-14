{-# LANGUAGE LambdaCase #-}
module Language.Cimple.PrettyComment
    ( ppCommentInfo
    ) where

import           Data.Fix                      (foldFix)
import           Data.List                     (dropWhile)
import qualified Data.List.Split               as List
import           Data.Text                     (Text)
import           Language.Cimple               (AssignOp (..), BinaryOp (..),
                                                Comment, CommentF (..),
                                                CommentStyle (..), Lexeme (..),
                                                LexemeClass (..), Node,
                                                NodeF (..), Nullability (..),
                                                Scope (..), UnaryOp (..),
                                                lexemeLine, lexemeText)
import           Language.Cimple.PrettyColor   (black, blue, cyan, dullcyan,
                                                dullgreen, dullmagenta, dullred,
                                                dullyellow, underline)
import           Language.Cimple.PrettyCommon
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)

ppCodeBody :: [Doc AnsiStyle] -> Doc AnsiStyle
ppCodeBody =
    vcat
    . map (pretty . (" *" <>))
    . dropWhile null
    . List.splitOn "\n"
    . renderS
    . plain
    . hcat

ppCommentInfo :: Comment (Lexeme Text) -> Doc AnsiStyle
ppCommentInfo = foldFix go
  where
  ppRef      = underline . cyan . ppLexeme
  ppAttr     = maybe mempty (blue . ppLexeme)
  mapTail _ []     = []
  mapTail f (x:xs) = x:map f xs

  go :: CommentF (Lexeme Text) (Doc AnsiStyle) -> Doc AnsiStyle
  go = \case
    DocComment docs ->
        dullyellow (pretty "/**") <>
        (if null docs then mempty else vcat (map align $ mapTail (pretty " *" <>) docs)) <>
        line <> dullyellow (pretty " */")

    DocWord w -> ppLexeme w

    DocParam attr name ->
        kwDocParam <> ppAttr attr <+> ppLexeme name

    DocSecurityRank kw rank ->
        kwDocSecurityRank <> pretty '(' <> ppLexeme kw <> pretty ", " <> ppLexeme rank <> pretty ')'

    DocAttention      -> kwDocAttention
    DocBrief          -> kwDocBrief
    DocDeprecated     -> kwDocDeprecated
    DocFile           -> kwDocFile
    DocReturn         -> kwDocReturn
    DocRetval         -> kwDocRetval
    DocSee name       -> kwDocSee        <+> ppRef name
    DocRef name         -> kwDocRef        <+> ppRef name
    DocP name           -> kwDocP          <+> ppRef name
    DocExtends feat     -> kwDocExtends    <+> ppLexeme feat
    DocImplements feat  -> kwDocImplements <+> ppLexeme feat
    DocPrivate          -> kwDocPrivate
    DocNote             -> kwDocNote
    DocSection title -> kwDocSection <+> ppLexeme title
    DocSubsection title -> kwDocSubsection <+> ppLexeme title

    DocLine docs -> hcat docs
    DocCode _ code _ ->
        kwDocCode <> line <> ppCodeBody code <> kwDocEndCode

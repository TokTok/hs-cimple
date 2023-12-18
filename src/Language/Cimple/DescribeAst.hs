{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.DescribeAst
    ( HasLocation (..)
    , describeLexeme
    , describeNode
    ) where

import           Data.Fix                (Fix (..), foldFix)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Language.Cimple.Ast     (Node, NodeF (..))
import qualified Language.Cimple.Flatten as Flatten
import           Language.Cimple.Lexer   (Lexeme, lexemeLine)


class HasLocation a where
    sloc :: FilePath -> a -> Text

instance HasLocation (Lexeme text) where
    sloc file l = Text.pack file <> ":" <> Text.pack (show (lexemeLine l))

instance HasLocation lexeme => HasLocation (Node lexeme) where
    sloc file n =
        case foldFix Flatten.lexemes n of
            []  -> Text.pack file <> ":0:0"
            l:_ -> sloc file l


describeNode :: Show a => Node a -> String
describeNode node = case unFix node of
    PreprocIf{}     -> "#if/#endif block"
    PreprocIfdef{}  -> "#ifdef/#endif block"
    PreprocIfndef{} -> "#ifndef/#endif block"
    _               -> show $ ellipsis <$ unFix node
  where
    ellipsis :: String
    ellipsis = "..."

describeLexeme :: Show a => Lexeme a -> String
describeLexeme = show

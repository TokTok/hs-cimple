{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Cimple.Diagnostics
  ( Diagnostics
  , HasDiagnostics (..)
  , warn
  , sloc
  , at
  ) where

import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Language.Cimple.AST      (Node)
import           Language.Cimple.Lexer    (AlexPosn (..), Lexeme (..),
                                           lexemeLine)
import           Language.Cimple.Tokens   (LexemeClass (..))

type DiagnosticsT diags a = State diags a
type Diagnostics a = DiagnosticsT [Text] a


class HasDiagnostics a where
    addDiagnostic :: Text -> a -> a

instance HasDiagnostics [Text] where
    addDiagnostic = (:)


warn :: HasDiagnostics diags => FilePath -> Lexeme Text -> Text -> DiagnosticsT diags ()
warn file l w = State.modify (addDiagnostic $ sloc file l <> ": " <> w)


sloc :: FilePath -> Lexeme text -> Text
sloc file l = Text.pack file <> ":" <> Text.pack (show (lexemeLine l))


at :: Node a (Lexeme Text) -> Lexeme Text
at n =
    case foldMap (:[]) n of
        []  -> L (AlexPn 0 0 0) Error "unknown source location"
        l:_ -> l

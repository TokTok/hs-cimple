{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Language.Cimple.Diagnostics
  ( Diagnostics
  , Diagnostics' -- deprecated
  , HasDiagnostics (..)
  , warn
  , warn' -- deprecated
  , sloc
  ) where

import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Fix                   (foldFix)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple.Ast        (Node)
import qualified Language.Cimple.Flatten    as Flatten
import           Language.Cimple.Lexer      (Lexeme (..), lexemeLine)

type DiagnosticsT diags a = State diags a
type Diagnostics a = DiagnosticsT [Text] a
type Diagnostics' a = Diagnostics a

warn, warn'
    :: (HasLocation at, HasDiagnostics diags)
    => FilePath -> at -> Text -> DiagnosticsT diags ()
warn file l w = State.modify (addDiagnostic $ sloc file l <> ": " <> w)
warn' = warn


class HasDiagnostics a where
    addDiagnostic :: Text -> a -> a

instance HasDiagnostics [Text] where
    addDiagnostic = (:)


class HasLocation a where
    sloc :: FilePath -> a -> Text

instance HasLocation (Lexeme text) where
    sloc file l = Text.pack file <> ":" <> Text.pack (show (lexemeLine l))

instance HasLocation lexeme => HasLocation (Node lexeme) where
    sloc file n =
        case foldFix Flatten.lexemes n of
            []  -> Text.pack file <> ":0:0"
            l:_ -> sloc file l

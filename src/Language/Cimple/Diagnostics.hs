{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Language.Cimple.Diagnostics
  ( Diagnostics
  , HasDiagnostics (..)
  , warn
  , sloc
  ) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Text                   (Text)
import           Language.Cimple.DescribeAst (HasLocation (..))

type DiagnosticsT diags a = State diags a
type Diagnostics a = DiagnosticsT [Text] a

warn
    :: (HasLocation at, HasDiagnostics diags)
    => FilePath -> at -> Text -> DiagnosticsT diags ()
warn file l w = State.modify (addDiagnostic $ sloc file l <> ": " <> w)


class HasDiagnostics a where
    addDiagnostic :: Text -> a -> a

instance HasDiagnostics [Text] where
    addDiagnostic = (:)

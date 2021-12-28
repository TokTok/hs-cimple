module Language.Cimple
    ( module X
    , AstActions
    , defaultActions
    ) where

import           Control.Monad.State.Lazy    (State)
import           Data.Text                   (Text)

import           Language.Cimple.Annot       as X
import           Language.Cimple.AST         as X
import           Language.Cimple.Lexer       as X
import           Language.Cimple.Parser      as X
import           Language.Cimple.Tokens      as X
import           Language.Cimple.TraverseAst as X

type AstActions a = X.IdentityActions (State a) Text

defaultActions :: AstActions state
defaultActions = X.identityActions

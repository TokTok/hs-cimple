module Language.Cimple
    ( module X
    , DefaultActions
    , defaultActions
    , removeSloc
    ) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Text                   (Text)

import           Language.Cimple.Annot       as X
import           Language.Cimple.Ast         as X
import           Language.Cimple.DescribeAst as X
import           Language.Cimple.Lexer       as X
import           Language.Cimple.MapAst      as X
import           Language.Cimple.Parser      as X
import           Language.Cimple.Tokens      as X

type DefaultActions a = X.IdentityActions (State a) Text

defaultActions :: DefaultActions state
defaultActions = X.identityActions

removeSloc :: Node (Lexeme Text) -> Node (Lexeme Text)
removeSloc =
    flip State.evalState () . mapAst defaultActions
        { doLexeme = \_ (L _ c t) _ -> pure $ L (AlexPn 0 0 0) c t }

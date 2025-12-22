module Language.Cimple
    ( module X
    , DefaultActions
    , defaultActions
    , removeSloc
    , elideGroups
    , getParamNameFromNode
    ) where

import           Control.Monad.State.Strict          (State)
import qualified Control.Monad.State.Strict          as State
import           Data.Fix                            (Fix (..))
import           Data.Text                           (Text)

import           Language.Cimple.Annot               as X
import           Language.Cimple.Ast                 as X
import           Language.Cimple.DescribeAst         as X
import           Language.Cimple.Flatten             as X
import           Language.Cimple.Lexer               as X
import           Language.Cimple.MapAst              as X
import           Language.Cimple.Parser              as X
import           Language.Cimple.Parser.Error.Pretty as X
import           Language.Cimple.Tokens              as X

type DefaultActions a = X.IdentityActions (State a) Text

defaultActions :: DefaultActions state
defaultActions = X.identityActions

removeSloc :: Node (Lexeme Text) -> Node (Lexeme Text)
removeSloc =
    flip State.evalState () . mapAst defaultActions
        { doLexeme = \_ (L _ c t) _ -> pure $ L (AlexPn 0 0 0) c t }

elideGroups :: Node (Lexeme Text) -> Node (Lexeme Text)
elideGroups =
    flip State.evalState () . mapAst defaultActions
        { doNodes = \_ _ m_nodes -> do
            nodes <- m_nodes
            return $ concatMap flatten nodes
        }
  where
    flatten (Fix (Group ns)) = concatMap flatten ns
    flatten n                = [n]

getParamNameFromNode :: Node (Lexeme Text) -> Maybe Text
getParamNameFromNode (Fix (VarDecl _ (L _ _ name) _)) = Just name
getParamNameFromNode _                                = Nothing

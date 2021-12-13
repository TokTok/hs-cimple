{-# LANGUAGE StrictData #-}
module Language.Cimple.Analysis.Includes
  ( collectIncludes
  , normaliseIncludes
  ) where

import           Control.Monad.State.Lazy        (State)
import qualified Control.Monad.State.Lazy        as State
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.Cimple.AST             (Node (..))
import           Language.Cimple.Lexer           (Lexeme (..))
import           Language.Cimple.Tokens          (LexemeClass (..))
import           Language.Cimple.TranslationUnit (TranslationUnit)
import           Language.Cimple.TraverseAst     (AstActions (..),
                                                  defaultActions, traverseAst)
import           System.FilePath                 (joinPath, splitPath,
                                                  takeDirectory)

collectIncludes
  :: [FilePath]
  -> TranslationUnit Text
  -> [FilePath]
  -> Either String ((), FilePath, [FilePath])
collectIncludes sources (file, _) includes =
    case filter (not . (`elem` sources)) includes of
        []        -> Right ((), file, includes)
        missing:_ -> Left $ file <> " includes missing " <> missing


relativeTo :: FilePath -> FilePath -> FilePath
relativeTo "." file = file
relativeTo dir file = go (splitPath dir) (splitPath file)
  where
    go d ("../":f) = go (init d) f
    go d f         = joinPath (d ++ f)


normaliseIncludes :: TranslationUnit Text -> (TranslationUnit Text, [FilePath])
normaliseIncludes (file, ast) =
    ((file, ast'), includes)
  where
    (ast', includes) = State.runState (traverseAst (go (takeDirectory file)) ast) []

    go :: FilePath -> AstActions (State [FilePath]) Text
    go dir = defaultActions
        { doNode = \node act ->
            case node of
                PreprocInclude (L spos LitString include) -> do
                    let includePath = relativeTo dir $ tread include
                    State.modify (includePath :)
                    return $ PreprocInclude (L spos LitString (tshow includePath))

                _ -> act
        }

      where
        tshow = Text.pack . show
        tread = read . Text.unpack

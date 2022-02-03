{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Language.Cimple.SemCheck.Includes
  ( collectIncludes
  , normaliseIncludes
  ) where

import           Control.Monad.State.Strict      (State)
import qualified Control.Monad.State.Strict      as State
import           Data.Fix                        (Fix (..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.Cimple.Ast             (NodeF (..))
import           Language.Cimple.Lexer           (Lexeme (..))
import           Language.Cimple.MapAst          (IdentityActions, doNode,
                                                  identityActions, mapAst)
import           Language.Cimple.Tokens          (LexemeClass (..))
import           Language.Cimple.TranslationUnit (TranslationUnit)
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


normaliseIncludes' :: FilePath -> IdentityActions (State [FilePath]) Text
normaliseIncludes' dir = identityActions
    { doNode = \_ node act ->
        case node of
            Fix (PreprocInclude (L spos LitString include)) -> do
                let includePath = relativeTo dir $ tread include
                State.modify (includePath :)
                return $ Fix $ PreprocInclude (L spos LitString (tshow includePath))

            _ -> act
    }

  where
    tshow = Text.pack . show
    tread = read . Text.unpack


normaliseIncludes :: TranslationUnit Text -> (TranslationUnit Text, [FilePath])
normaliseIncludes (file, ast) =
    ((file, ast'), includes)
  where
    (ast', includes) = State.runState (mapAst (normaliseIncludes' (takeDirectory file)) ast) []

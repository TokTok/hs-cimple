{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Language.Cimple.Program
  ( Program
  , fromList
  , toList
  , includeGraph
  ) where

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Text                         (Text)
import           Language.Cimple.AST               (Node)
import           Language.Cimple.Graph             (Graph)
import qualified Language.Cimple.Graph             as Graph
import           Language.Cimple.Lexer             (Lexeme (..))
import           Language.Cimple.SemCheck.Includes (collectIncludes,
                                                    normaliseIncludes)
import           Language.Cimple.TranslationUnit   (TranslationUnit)


data Program text = Program
  { progAsts     :: Map FilePath [Node (Lexeme text)]
  , progIncludes :: Graph () FilePath
  }


toList :: Program a -> [TranslationUnit a]
toList = Map.toList . progAsts


includeGraph :: Program a -> [(FilePath, FilePath)]
includeGraph = Graph.edges . progIncludes


fromList :: [TranslationUnit Text] -> Either String (Program Text)
fromList tus = do
    let tusWithIncludes = map normaliseIncludes tus
    let progAsts = Map.fromList . map fst $ tusWithIncludes
    -- Check whether all includes can be resolved.
    includeEdges <- mapM (uncurry $ collectIncludes $ map fst tus) tusWithIncludes
    let progIncludes = Graph.fromEdges includeEdges
    return Program{..}

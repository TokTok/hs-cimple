{-# LANGUAGE StrictData #-}
module Language.Cimple.Program
  ( Program
  , TranslationUnit
  , fromList
  , toList
  ) where

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Language.Cimple.AST   (Node (..))
import           Language.Cimple.Lexer (Lexeme)


type TranslationUnit a = (FilePath, [Node (Lexeme a)])

data Program a = Program
  { progAsts :: Map FilePath [Node (Lexeme a)]
  }


fromList :: [TranslationUnit a] -> Program a
fromList = Program . Map.fromList


toList :: Program a -> [TranslationUnit a]
toList = Map.toList . progAsts

module Tokstyle.Cimple.Analysis (analyse) where

import           Data.Text                            (Text)
import           Tokstyle.Cimple.AST                  (Node (..))
import           Tokstyle.Cimple.Lexer                (Lexeme)

import qualified Tokstyle.Cimple.Analysis.GlobalFuncs as GlobalFuncs

analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file ast = concatMap (\f -> f file ast)
    [ GlobalFuncs.analyse
    ]

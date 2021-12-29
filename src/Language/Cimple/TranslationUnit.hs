{-# LANGUAGE StrictData #-}
module Language.Cimple.TranslationUnit
  ( TranslationUnit
  ) where

import           Language.Cimple.Ast   (Node)
import           Language.Cimple.Lexer (Lexeme)

type TranslationUnit text = (FilePath, [Node (Lexeme text)])

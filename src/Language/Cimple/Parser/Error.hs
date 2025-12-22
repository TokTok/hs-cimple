{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.Parser.Error
    ( parseError
    ) where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Text                  (Text)
import           Language.Cimple.Lexer      (Alex, Lexeme (..), ParseError (..),
                                             alexError, getContext)
import           Language.Cimple.Tokens     (LexemeClass (..))

parseError :: (Lexeme Text, [String]) -> Alex a
parseError (l@(L p _ _), options) = do
    ctx <- getContext
    alexError $ LBSC.unpack (encode (ParseError p ctx options l))

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.DescribeAst
    ( HasLocation (..)
    , describeLexeme
    , describeNode
    , parseError
    ) where

import           Data.Fix                (Fix (..), foldFix)
import           Data.List               (isPrefixOf, (\\))
import qualified Data.List               as List
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Language.Cimple.Ast     (Node, NodeF (..))
import qualified Language.Cimple.Flatten as Flatten
import           Language.Cimple.Lexer   (Alex, AlexPosn (..), Lexeme (..),
                                          alexError, lexemeLine)
import           Language.Cimple.Tokens  (LexemeClass (..))


class HasLocation a where
    sloc :: FilePath -> a -> Text

instance HasLocation (Lexeme text) where
    sloc file l = Text.pack file <> ":" <> Text.pack (show (lexemeLine l))

instance HasLocation lexeme => HasLocation (Node lexeme) where
    sloc file n =
        case foldFix Flatten.lexemes n of
            []  -> Text.pack file <> ":0:0"
            l:_ -> sloc file l


describeNode :: Show a => Node a -> String
describeNode node = case unFix node of
    PreprocIf{}     -> "#if/#endif block"
    PreprocIfdef{}  -> "#ifdef/#endif block"
    PreprocIfndef{} -> "#ifndef/#endif block"
    _               -> show $ ellipsis <$ unFix node
  where
    ellipsis :: String
    ellipsis = "..."

describeLexemeClass :: LexemeClass -> Maybe String
describeLexemeClass = d
  where
    d IdConst               = Just "constant name"
    d IdFuncType            = Just "function type name"
    d IdStdType             = Just "standard type name"
    d IdSueType             = Just "type name"
    d IdVar                 = Just "variable name"
    d LitChar               = Just "character literal"
    d LitInteger            = Just "integer literal"
    d LitString             = Just "string literal"
    d LitSysInclude         = Just "system include"
    d PctAmpersand          = Just "address-of or bitwise-and operator"
    d PctAmpersandAmpersand = Just "logical-and operator"
    d PctAmpersandEq        = Just "bitwise-and-assign operator"
    d PctArrow              = Just "pointer-member-access operator"
    d PctAsterisk           = Just "pointer-type, dereference, or multiply operator"
    d PctAsteriskEq         = Just "multiply-assign operator"
    d PctCaret              = Just "bitwise-xor operator"
    d PctCaretEq            = Just "xor-assign operator"
    d PctColon              = Just "ternary operator"
    d PctComma              = Just "comma"
    d PctEllipsis           = Just "ellipsis"
    d PctEMark              = Just "logical not operator"
    d PctEMarkEq            = Just "not-equals operator"
    d PctEq                 = Just "assignment operator"
    d PctEqEq               = Just "equals operator"
    d PctGreater            = Just "greater-than operator"
    d PctGreaterEq          = Just "greater-or-equals operator"
    d PctGreaterGreater     = Just "right-shift operator"
    d PctGreaterGreaterEq   = Just "right-shift-assign operator"
    d PctLBrace             = Just "left brace"
    d PctLBrack             = Just "left square bracket"
    d PctLess               = Just "less-than operator"
    d PctLessEq             = Just "less-or-equals operator"
    d PctLessLess           = Just "left-shift operator"
    d PctLessLessEq         = Just "left-shift-assign operator"
    d PctLParen             = Just "left parenthesis"
    d PctMinus              = Just "minus operator"
    d PctMinusEq            = Just "minus-assign operator"
    d PctMinusMinus         = Just "decrement operator"
    d PctPeriod             = Just "member access operator"
    d PctPercent            = Just "modulus operator"
    d PctPercentEq          = Just "modulus-assign operator"
    d PctPipe               = Just "bitwise-or operator"
    d PctPipeEq             = Just "bitwise-or-assign operator"
    d PctPipePipe           = Just "logical-or operator"
    d PctPlus               = Just "addition operator"
    d PctPlusEq             = Just "add-assign operator"
    d PctPlusPlus           = Just "increment operator"
    d PctQMark              = Just "ternary operator"
    d PctRBrace             = Just "right brace"
    d PctRBrack             = Just "right square bracket"
    d PctRParen             = Just "right parenthesis"
    d PctSemicolon          = Just "end of statement semicolon"
    d PctSlash              = Just "division operator"
    d PctSlashEq            = Just "divide-assign operator"
    d PctTilde              = Just "bitwise-not operator"
    d PpDefine              = Just "preprocessor define"
    d PpDefined             = Just "preprocessor defined"
    d PpElif                = Just "preprocessor elif"
    d PpElse                = Just "preprocessor else"
    d PpEndif               = Just "preprocessor endif"
    d PpIf                  = Just "preprocessor if"
    d PpIfdef               = Just "preprocessor ifdef"
    d PpIfndef              = Just "preprocessor ifndef"
    d PpInclude             = Just "preprocessor include"
    d PpNewline             = Just "newline"
    d PpUndef               = Just "preprocessor undef"
    d CmtBlock              = Just "block comment"
    d CmtCommand            = Just "doxygen command"
    d CmtAttr               = Just "parameter attribute"
    d CmtEndDocSection      = Just "doxygen end-of-section"
    d CmtIndent             = Just "indented comment"
    d CmtStart              = Just "start of comment"
    d CmtStartCode          = Just "escaped comment"
    d CmtStartBlock         = Just "block comment"
    d CmtStartDoc           = Just "doxygen comment"
    d CmtStartDocSection    = Just "doxygen start-of-section"
    d CmtSpdxCopyright      = Just "SPDX Copyright"
    d CmtSpdxLicense        = Just "SPDX License"
    d CmtCode               = Just "code comment"
    d CmtWord               = Just "comment word"
    d CmtRef                = Just "comment reference"
    d CmtEnd                = Just "end of comment"
    d IgnStart              = Just "tokstyle ignore start"
    d IgnBody               = Just "tokstyle ignored code"
    d IgnEnd                = Just "tokstyle ignore end"

    d ErrorToken            = Just "lexical error"
    d Eof                   = Just "end-of-file"
    d _                     = Nothing

describeLexeme :: Show a => Lexeme a -> String
describeLexeme (L _ c s) = maybe "" (<> ": ") (describeLexemeClass c) <> show s

describeExpected :: [String] -> String
describeExpected [] = "end of file"
describeExpected ["ID_VAR"] = "variable name"
describeExpected [option] = option
describeExpected options
    | wants ["break", "const", "continue", "ID_CONST", "VLA"] = "statement or declaration"
    | wants ["ID_FUNC_TYPE", "non_null", "static", "'#include'"] = "top-level declaration or definition"
    | options == ["ID_STD_TYPE", "ID_SUE_TYPE", "struct", "void"] = "type specifier"
    | options == ["ID_STD_TYPE", "ID_SUE_TYPE", "const", "struct", "void"] = "type specifier"
    | ["ID_FUNC_TYPE", "ID_STD_TYPE", "ID_SUE_TYPE", "ID_VAR"] `isPrefixOf` options = "type specifier or variable name"
    | ["ID_FUNC_TYPE", "ID_STD_TYPE", "ID_SUE_TYPE", "const"] `isPrefixOf` options = "type specifier"
    | ["ID_CONST", "sizeof", "LIT_CHAR", "LIT_FALSE", "LIT_TRUE", "LIT_INTEGER"] `isPrefixOf` options = "constant expression"
    | ["ID_CONST", "ID_SUE_TYPE", "'/*'"] `isPrefixOf` options = "enumerator, type name, or comment"
    | wants ["'defined'"] = "preprocessor constant expression"
    | wants ["'&'", "'&&'", "'*'", "'=='", "';'"] = "operator or end of statement"
    | wants ["'&'", "'&&'", "'*'", "'^'", "'!='"] = "operator"
    | wants ["ID_CONST", "ID_VAR", "sizeof", "LIT_CHAR", "'--'", "'&'", "'*'"] = "expression"
    | ["ID_CONST", "ID_STD_TYPE", "ID_SUE_TYPE", "ID_VAR", "const", "sizeof"] `isPrefixOf` options = "expression or type specifier"
    | ["ID_CONST", "ID_STD_TYPE", "ID_SUE_TYPE", "const", "sizeof"] `isPrefixOf` options = "constant expression or type specifier"
    | ["'&='", "'->'", "'*='"] `isPrefixOf` options = "assignment or member/array access"
    | wants ["CMT_WORD"] = "comment contents"

    | length options == 2 = commaOr options
    | otherwise           = "one of " <> commaOr options
  where
    wants xs = null (xs \\ options)

commaOr :: [String] -> String
commaOr = go . reverse
  where
    go []     = ""
    go (x:xs) = List.intercalate ", " (reverse xs) <> " or " <> x

parseError :: Show text => (Lexeme text, [String]) -> Alex a
parseError (l@(L (AlexPn _ line col) _ _), options) =
    alexError $ ":" <> show line <> ":" <> show col <> ": Parse error near " <> describeLexeme l
        <> "; expected " <> describeExpected options

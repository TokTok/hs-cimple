{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.DescribeAst
    ( HasLocation (..)
    , describeLexeme
    , describeNode
    ) where

import           Data.Fix                (Fix (..), foldFix)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Language.Cimple.Ast     (Node, NodeF (..))
import qualified Language.Cimple.Flatten as Flatten
import           Language.Cimple.Lexer   (Lexeme (..), lexemeLine)
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

describeLexemeClass :: LexemeClass -> String
describeLexemeClass = d
  where
    d IdConst               = "constant name"
    d IdFuncType            = "function type name"
    d IdStdType             = "standard type name"
    d IdSueType             = "type name"
    d IdVar                 = "variable name"
    d KwBreak               = "break"
    d KwCase                = "case"
    d KwConst               = "const"
    d KwContinue            = "continue"
    d KwDefault             = "default"
    d KwDo                  = "do"
    d KwElse                = "else"
    d KwEnum                = "enum"
    d KwExtern              = "extern"
    d KwFor                 = "for"
    d KwGnuPrintf           = "gnu_printf"
    d KwGoto                = "goto"
    d KwIf                  = "if"
    d KwNonNull             = "non_null"
    d KwNullable            = "nullable"
    d KwReturn              = "return"
    d KwSizeof              = "sizeof"
    d KwStatic              = "static"
    d KwStaticAssert        = "static_assert"
    d KwStruct              = "struct"
    d KwSwitch              = "switch"
    d KwTypedef             = "typedef"
    d KwUnion               = "union"
    d KwVla                 = "VLA"
    d KwVoid                = "void"
    d KwWhile               = "while"
    d LitFalse              = "false"
    d LitTrue               = "true"
    d LitChar               = "character literal"
    d LitInteger            = "integer literal"
    d LitString             = "string literal"
    d LitSysInclude         = "system include"
    d PctAmpersand          = "address-of or bitwise-and operator"
    d PctAmpersandAmpersand = "logical-and operator"
    d PctAmpersandEq        = "bitwise-and-assign operator"
    d PctArrow              = "pointer-member-access operator"
    d PctAsterisk           = "dereference or multiply operator"
    d PctAsteriskEq         = "multiply-assign operator"
    d PctCaret              = "bitwise-xor operator"
    d PctCaretEq            = "xor-assign operator"
    d PctColon              = "ternary operator"
    d PctComma              = "comma"
    d PctEllipsis           = "ellipsis"
    d PctEMark              = "logical not operator"
    d PctEMarkEq            = "not-equals operator"
    d PctEq                 = "assignment operator"
    d PctEqEq               = "equals operator"
    d PctGreater            = "greater-than operator"
    d PctGreaterEq          = "greater-or-equals operator"
    d PctGreaterGreater     = "right-shift operator"
    d PctGreaterGreaterEq   = "right-shift-assign operator"
    d PctLBrace             = "left brace"
    d PctLBrack             = "left square bracket"
    d PctLess               = "less-than operator"
    d PctLessEq             = "less-or-equals operator"
    d PctLessLess           = "left-shift operator"
    d PctLessLessEq         = "left-shift-assign operator"
    d PctLParen             = "left parenthesis"
    d PctMinus              = "minus operator"
    d PctMinusEq            = "minus-assign operator"
    d PctMinusMinus         = "decrement operator"
    d PctPeriod             = "member access operator"
    d PctPercent            = "modulus operator"
    d PctPercentEq          = "modulus-assign operator"
    d PctPipe               = "bitwise-or operator"
    d PctPipeEq             = "bitwise-or-assign operator"
    d PctPipePipe           = "logical-or operator"
    d PctPlus               = "addition operator"
    d PctPlusEq             = "add-assign operator"
    d PctPlusPlus           = "increment operator"
    d PctQMark              = "ternary operator"
    d PctRBrace             = "right brace"
    d PctRBrack             = "right square bracket"
    d PctRParen             = "right parenthesis"
    d PctSemicolon          = "end of statement semicolon"
    d PctSlash              = "division operator"
    d PctSlashEq            = "divide-assign operator"
    d PctTilde              = "bitwise-not operator"
    d PpDefine              = "preprocessor define"
    d PpDefined             = "preprocessor defined"
    d PpElif                = "preprocessor elif"
    d PpElse                = "preprocessor else"
    d PpEndif               = "preprocessor endif"
    d PpIf                  = "preprocessor if"
    d PpIfdef               = "preprocessor ifdef"
    d PpIfndef              = "preprocessor ifndef"
    d PpInclude             = "preprocessor include"
    d PpNewline             = "newline"
    d PpUndef               = "preprocessor undef"
    d CmtBlock              = "block comment"
    d CmtCommand            = "doxygen command"
    d CmtAttr               = "parameter attribute"
    d CmtEndDocSection      = "doxygen end-of-section"
    d CmtIndent             = "indented comment"
    d CmtStart              = "start of comment"
    d CmtStartCode          = "escaped comment"
    d CmtStartBlock         = "block comment"
    d CmtStartDoc           = "doxygen comment"
    d CmtStartDocSection    = "doxygen start-of-section"
    d CmtSpdxCopyright      = "SPDX Copyright"
    d CmtSpdxLicense        = "SPDX License"
    d CmtCode               = "code comment"
    d CmtWord               = "comment word"
    d CmtRef                = "comment reference"
    d CmtEnd                = "end of comment"
    d IgnStart              = "tokstyle ignore start"
    d IgnBody               = "tokstyle ignored code"
    d IgnEnd                = "tokstyle ignore end"

    d ErrorToken            = "lexical error"
    d Eof                   = "end-of-file"

describeLexeme :: Show a => Lexeme a -> String
describeLexeme (L _ c s) = describeLexemeClass c <> ": " <> show s

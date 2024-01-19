{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.CimpleSpec where

import           Test.Hspec      (Spec, describe, it, shouldNotBe)

import           Data.Text       (Text)
import           Language.Cimple (LexemeClass (..))
import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum, forAll,
                                  suchThat)

instance Arbitrary LexemeClass where
    arbitrary = arbitraryBoundedEnum `suchThat` ok
      where
        ok ErrorToken = False
        ok Eof        = False
        ok _          = True


sampleToken :: LexemeClass -> Text
sampleToken c = case c of
    IdConst               -> "ID_CONST"
    IdFuncType            -> "func_cb"
    IdStdType             -> "uint32_t"
    IdSueType             -> "Sue_Type"
    IdVar                 -> "var"
    KwBreak               -> "break"
    KwCase                -> "case"
    KwConst               -> "const"
    KwContinue            -> "continue"
    KwDefault             -> "default"
    KwDo                  -> "do"
    KwElse                -> "else"
    KwEnum                -> "enum"
    KwExtern              -> "extern"
    KwFor                 -> "for"
    KwGnuPrintf           -> "gnu_printf"
    KwGoto                -> "goto"
    KwIf                  -> "if"
    KwNonNull             -> "non_null"
    KwNullable            -> "nullable"
    KwReturn              -> "return"
    KwSizeof              -> "sizeof"
    KwStatic              -> "static"
    KwStaticAssert        -> "static_assert"
    KwStruct              -> "struct"
    KwSwitch              -> "switch"
    KwTypedef             -> "typedef"
    KwUnion               -> "union"
    KwVla                 -> "VLA"
    KwVoid                -> "void"
    KwWhile               -> "while"
    LitFalse              -> "false"
    LitTrue               -> "true"
    LitChar               -> "'a'"
    LitInteger            -> "123"
    LitString             -> "\"str\""
    LitSysInclude         -> "<stdio.h>"
    PctAmpersand          -> "&"
    PctAmpersandAmpersand -> "&&"
    PctAmpersandEq        -> "&="
    PctArrow              -> "->"
    PctAsterisk           -> "*"
    PctAsteriskEq         -> "*="
    PctCaret              -> "^"
    PctCaretEq            -> "^="
    PctColon              -> ":"
    PctComma              -> ","
    PctEllipsis           -> "..."
    PctEMark              -> "!"
    PctEMarkEq            -> "!="
    PctEq                 -> "="
    PctEqEq               -> "=="
    PctGreater            -> ">"
    PctGreaterEq          -> ">="
    PctGreaterGreater     -> ">>"
    PctGreaterGreaterEq   -> ">>="
    PctLBrace             -> "{\n"
    PctLBrack             -> "["
    PctLess               -> "<"
    PctLessEq             -> "<="
    PctLessLess           -> "<<"
    PctLessLessEq         -> "<<="
    PctLParen             -> "("
    PctMinus              -> "-"
    PctMinusEq            -> "-="
    PctMinusMinus         -> "--"
    PctPeriod             -> "."
    PctPercent            -> "%"
    PctPercentEq          -> "%="
    PctPipe               -> "|"
    PctPipeEq             -> "|="
    PctPipePipe           -> "||"
    PctPlus               -> "+"
    PctPlusEq             -> "+="
    PctPlusPlus           -> "++"
    PctQMark              -> "?"
    PctRBrace             -> "}"
    PctRBrack             -> "]"
    PctRParen             -> ")"
    PctSemicolon          -> ";\n"
    PctSlash              -> "/"
    PctSlashEq            -> "/="
    PctTilde              -> "~"
    PpDefine              -> "\n#define"
    PpDefined             -> "\n#defined"
    PpElif                -> "\n#elif"
    PpElse                -> "\n#else"
    PpEndif               -> "\n#endif"
    PpIf                  -> "\n#if"
    PpIfdef               -> "\n#ifdef"
    PpIfndef              -> "\n#ifndef"
    PpInclude             -> "\n#include"
    PpNewline             -> "\n"
    PpUndef               -> "\n#undef"
    CmtBlock              -> "/**"
    CmtCommand            -> "@param"
    CmtAttr               -> "[out]"
    CmtEndDocSection      -> "/** @} */"
    CmtIndent             -> "*"
    CmtStart              -> "/*"
    CmtStartCode          -> "/*!"
    CmtStartBlock         -> "/***"
    CmtStartDoc           -> "/**"
    CmtStartDocSection    -> "/** @{"
    CmtSpdxCopyright      -> "Copyright ©"
    CmtSpdxLicense        -> "SPDX-License-Identifier:"
    CmtCode               -> "@code"
    CmtWord               -> "hello"
    CmtRef                -> "`ref`"
    CmtEnd                -> "*/\n"
    IgnStart              -> "\n//!TOKSTYLE-\n"
    IgnBody               -> "ignored stuff"
    IgnEnd                -> "\n//!TOKSTYLE+\n"

    ErrorToken            -> "!!ERROR!!"
    Eof                   -> "!!EOF!!"

spec :: Spec
spec = do
    describe "tokens" $ do
        it "can be turned into strings" $
            forAll arbitraryBoundedEnum $ \token ->
                sampleToken token `shouldNotBe` ""

{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.ParserSpec where

import qualified Data.ByteString.Lazy     as LBS
import           Data.Fix                 (Fix (..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Language.Cimple          (AlexPosn (..), Lexeme (..),
                                           LexemeClass (..), NodeF (..),
                                           Scope (..))
import qualified Language.Cimple          as Cimple
import           Language.Cimple.IO       (parseText)
import qualified Language.Happy           as Happy
import           Language.Happy.Arbitrary (Config, defConfig, genTokens)
import           Test.Hspec               (Spec, describe, it, shouldBe,
                                           shouldNotBe, shouldSatisfy)
import           Test.QuickCheck          (Gen, forAll)


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
    CmtPrefix             -> "//"
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


config :: Config LexemeClass
config = defConfig parseToken
  where
    parseToken :: Text -> LexemeClass
    parseToken =
        read
        . Text.unpack
        . (!! 2)
        . concatMap (filter (not . Text.null) . Text.splitOn "\t")
        . Text.splitOn " "

grammar :: Maybe Happy.Grammar
grammar = do
    source <- Cimple.source
    case Happy.runAlex (LBS.fromStrict source) Happy.parseGrammar of
        Left err -> error err
        Right ok -> return ok

arbitraryCode :: Happy.Grammar -> Gen Text
arbitraryCode g =
    Text.intercalate " " . map sampleToken <$> genTokens config "TranslationUnit" g

arbitrarySpec :: Spec
arbitrarySpec = case grammar of
    Nothing -> return ()
    Just g ->
        it "can handle arbitrary code" $
            forAll (arbitraryCode g) $ \code -> do
                case parseText code of
                    Right _  -> return ()
                    Left err -> err `shouldNotBe` ""


isRight1 :: Either a [b] -> Bool
isRight1 (Right [_]) = True
isRight1 _           = False


spec :: Spec
spec = do
    describe "C parsing" $ do
        arbitrarySpec

        it "should parse a simple function" $ do
            let ast = parseText "int a(void) { return 3; }"
            ast `shouldSatisfy` isRight1

        it "should parse a type declaration" $ do
            let ast = parseText "typedef struct Foo { int x; } Foo;"
            ast `shouldSatisfy` isRight1

        it "should parse a struct with bit fields" $ do
            let ast = parseText "typedef struct Foo { int x : 123; } Foo;"
            ast `shouldSatisfy` isRight1

        it "should parse a comment" $ do
            let ast = parseText "/* hello */"
            ast `shouldSatisfy` isRight1

        it "supports single declarators" $ do
            let ast = parseText "int main() { int a; }"
            ast `shouldBe` Right
                [ Fix (FunctionDefn Global
                    (Fix (FunctionPrototype
                        (Fix (TyStd (L (AlexPn 0 1 1) IdStdType "int")))
                        (L (AlexPn 4 1 5) IdVar "main")
                        []))
                    (Fix (CompoundStmt
                        [ Fix (VarDeclStmt
                            (Fix (VarDecl
                              (Fix (TyStd (L (AlexPn 13 1 14) IdStdType "int")))
                              (L (AlexPn 17 1 18) IdVar "a")
                              [])) Nothing)])))
                ]

        it "does not support multiple declarators per declaration" $ do
            let ast = parseText "int main() { int a, b; }"
            ast `shouldBe` Left
                ":1:19: Parse error near PctComma: \",\"; expected one of [\"'='\",\"';'\"]"

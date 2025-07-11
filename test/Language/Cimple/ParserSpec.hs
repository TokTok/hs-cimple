{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.ParserSpec where

import           Data.Fix           (Fix (..))
import           Test.Hspec         (Spec, describe, it, shouldBe,
                                     shouldSatisfy)

import           Language.Cimple    (AlexPosn (..), Lexeme (..),
                                     LexemeClass (..), NodeF (..), Scope (..))
import           Language.Cimple.IO (parseText)


isRight1 :: Either a [b] -> Bool
isRight1 (Right [_]) = True
isRight1 _           = False


spec :: Spec
spec = do
    describe "C parsing" $ do
        it "should parse a simple function" $ do
            let ast = parseText "int a(void) { return 3; }"
            ast `shouldSatisfy` isRight1

        it "should parse non_null annotations" $ do
            let ast = parseText "non_null() int a(char *p);"
            ast `shouldSatisfy` isRight1

        it "should parse non_null and nullable annotations" $ do
            let ast = parseText "non_null(2) nullable(1) int a(char *p, int *q);"
            ast `shouldSatisfy` isRight1

        it "should parse per-param non_null and nullable annotations" $ do
            let ast = parseText "int a(non_null() char *p, non_null() int *q);"
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

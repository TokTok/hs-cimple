{-# LANGUAGE OverloadedStrings #-}
module Language.CimpleSpec where

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Test.Hspec         (Spec, describe, it, shouldBe)

import           Language.Cimple    (AlexPosn (..), CommentStyle (..),
                                     Lexeme (..), LexemeClass (..),
                                     LiteralType (..), Node (..), Scope (..),
                                     TextActions, mapAst, textActions)
import           Language.Cimple.IO (parseText)


spec :: Spec
spec = do
    describe "TraverseAst" $ do
        it "should map the same way as mapM" $ do
            let Right ast = parseText "int a(void) { return 3; }"
            let actions :: TextActions Maybe () Text String
                actions = textActions (Just . Text.unpack)
            mapM (mapM (mapM (Just . Text.unpack))) ast
                `shouldBe`
                mapAst actions ast

    describe "C parsing" $ do
        it "should parse a simple function" $ do
            let ast = parseText "int a(void) { return 3; }"
            ast `shouldBe` Right
                [ FunctionDefn
                      Global
                      (FunctionPrototype
                          (TyStd (L (AlexPn 0 1 1) IdStdType "int"))
                          (L (AlexPn 4 1 5) IdVar "a")
                          [TyStd (L (AlexPn 6 1 7) KwVoid "void")]
                      )
                      (CompoundStmt [ Return
                            (Just
                                (LiteralExpr
                                    Int
                                    (L (AlexPn 21 1 22) LitInteger "3")
                                )
                            )
                      ])
                ]

        it "should parse a type declaration" $ do
            let ast = parseText "typedef struct Foo { int x; } Foo;"
            ast `shouldBe` Right
                [ Typedef
                      (Struct
                          (L (AlexPn 15 1 16) IdSueType "Foo")
                          [ MemberDecl
                                (TyStd (L (AlexPn 21 1 22) IdStdType "int"))
                                (DeclSpecVar (L (AlexPn 25 1 26) IdVar "x"))
                                Nothing
                          ]
                      )
                      (L (AlexPn 30 1 31) IdSueType "Foo")
                ]

        it "should parse a struct with bit fields" $ do
            let ast = parseText "typedef struct Foo { int x : 123; } Foo;"
            ast `shouldBe` Right
                [ Typedef
                      (Struct
                          (L (AlexPn 15 1 16) IdSueType "Foo")
                          [ MemberDecl
                                (TyStd (L (AlexPn 21 1 22) IdStdType "int"))
                                (DeclSpecVar (L (AlexPn 25 1 26) IdVar "x"))
                                (Just (L (AlexPn 29 1 30) LitInteger "123"))
                          ]
                      )
                      (L (AlexPn 36 1 37) IdSueType "Foo")
                ]

        it "should parse a comment" $ do
            let ast = parseText "/* hello */"
            ast `shouldBe` Right
                [ Comment Regular
                          (L (AlexPn 0 1 1) CmtStart "/*")
                          [L (AlexPn 3 1 4) CmtWord "hello"]
                          (L (AlexPn 9 1 10) CmtEnd "*/")
                ]

        it "supports single declarators" $ do
            let ast = parseText "int main() { int a; }"
            ast `shouldBe` Right
                [ FunctionDefn
                      Global
                      (FunctionPrototype
                          (TyStd (L (AlexPn 0 1 1) IdStdType "int"))
                          (L (AlexPn 4 1 5) IdVar "main")
                          []
                      )
                      (CompoundStmt [ VarDecl
                            (TyStd (L (AlexPn 13 1 14) IdStdType "int"))
                            (Declarator
                                (DeclSpecVar (L (AlexPn 17 1 18) IdVar "a"))
                                Nothing
                            )
                      ])
                ]

        it "does not support multiple declarators per declaration" $ do
            let ast = parseText "int main() { int a, b; }"
            ast `shouldBe` Left
                "1:19: Parse error near PctComma: \",\"; expected one of [\"';'\"]"

{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.ParserSpec where

import           Data.Fix           (Fix (..))
import           Test.Hspec         (Spec, describe, it, shouldBe)

import           Language.Cimple    (AlexPosn (..), CommentStyle (..),
                                     Lexeme (..), LexemeClass (..),
                                     LiteralType (..), NodeF (..), Scope (..))
import           Language.Cimple.IO (parseText)


spec :: Spec
spec = do
    describe "C parsing" $ do
        it "should parse a simple function" $ do
            let ast = parseText "int a(void) { return 3; }"
            ast `shouldBe` Right
                [ Fix (FunctionDefn
                      Global
                      (Fix (FunctionPrototype
                          (Fix (TyStd (L (AlexPn 0 1 1) IdStdType "int")))
                          (L (AlexPn 4 1 5) IdVar "a")
                          [Fix (TyStd (L (AlexPn 6 1 7) KwVoid "void"))]
                      ))
                      (Fix (CompoundStmt [ Fix (Return
                            (Just
                                (Fix (LiteralExpr
                                    Int
                                    (L (AlexPn 21 1 22) LitInteger "3")
                                ))
                            ))
                      ])))
                ]

        it "should parse a type declaration" $ do
            let ast = parseText "typedef struct Foo { int x; } Foo;"
            ast `shouldBe` Right
                [ Fix (Typedef
                      (Fix (Struct
                          (L (AlexPn 15 1 16) IdSueType "Foo")
                          [ Fix (MemberDecl
                                (Fix (TyStd (L (AlexPn 21 1 22) IdStdType "int")))
                                (Fix (DeclSpecVar (L (AlexPn 25 1 26) IdVar "x")))
                                Nothing)
                          ]
                      ))
                      (L (AlexPn 30 1 31) IdSueType "Foo"))
                ]

        it "should parse a struct with bit fields" $ do
            let ast = parseText "typedef struct Foo { int x : 123; } Foo;"
            ast `shouldBe` Right
                [ Fix (Typedef
                      (Fix (Struct
                          (L (AlexPn 15 1 16) IdSueType "Foo")
                          [ Fix (MemberDecl
                                (Fix (TyStd (L (AlexPn 21 1 22) IdStdType "int")))
                                (Fix (DeclSpecVar (L (AlexPn 25 1 26) IdVar "x")))
                                (Just (L (AlexPn 29 1 30) LitInteger "123")))
                          ]
                      ))
                      (L (AlexPn 36 1 37) IdSueType "Foo"))
                ]

        it "should parse a comment" $ do
            let ast = parseText "/* hello */"
            ast `shouldBe` Right
                [ Fix (Comment Regular
                          (L (AlexPn 0 1 1) CmtStart "/*")
                          [L (AlexPn 3 1 4) CmtWord "hello"]
                          (L (AlexPn 9 1 10) CmtEnd "*/"))
                ]

        it "supports single declarators" $ do
            let ast = parseText "int main() { int a; }"
            ast `shouldBe` Right
                [ Fix (FunctionDefn
                      Global
                      (Fix (FunctionPrototype
                          (Fix (TyStd (L (AlexPn 0 1 1) IdStdType "int")))
                          (L (AlexPn 4 1 5) IdVar "main")
                          []
                      ))
                      (Fix (CompoundStmt [ Fix (VarDecl
                            (Fix (TyStd (L (AlexPn 13 1 14) IdStdType "int")))
                            (Fix (Declarator
                                (Fix (DeclSpecVar (L (AlexPn 17 1 18) IdVar "a")))
                                Nothing
                            )))
                      ])))
                ]

        it "does not support multiple declarators per declaration" $ do
            let ast = parseText "int main() { int a, b; }"
            ast `shouldBe` Left
                "1:19: Parse error near PctComma: \",\"; expected one of [\"';'\"]"

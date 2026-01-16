{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Language.Cimple.AstSpec where

import           Data.Bifunctor       (Bifunctor (..))
import           Data.Fix             (Fix (..))
import           Data.Functor.Compose (Compose (..))
import           Test.Hspec           (Spec, describe, it, shouldBe,
                                       shouldSatisfy)

import           Language.Cimple      (AlexPosn (..), AnnotF (..),
                                       AssignOp (..), BinaryOp (..),
                                       CommentF (..), CommentStyle (..),
                                       Lexeme (..), LexemeClass (..),
                                       LiteralType (..), NodeF (..),
                                       Nullability (..), Scope (..),
                                       UnaryOp (..), addAnnot, removeAnnot)
import           Language.Cimple.IO   (parseText)


spec :: Spec
spec = do
    describe "Node" $ do
        it "can be annotated" $ do
            let Right [ast] = parseText "const int a = 3;"
            addAnnot ast `shouldBe`
                Fix (Compose (Annot () (
                    ConstDefn Global
                        (Fix (Compose (Annot () (
                            TyConst (Fix (Compose (Annot () (
                                TyStd (L (AlexPn 6 1 7) IdStdType "int")))))))))
                        (L (AlexPn 10 1 11) IdVar "a")
                        (Fix (Compose (Annot () (
                            LiteralExpr Int (L (AlexPn 14 1 15) LitInteger "3"))))))))

            removeAnnot (addAnnot ast) `shouldBe` ast

            addAnnot ast `shouldSatisfy` \case
                (removeAnnot -> Fix (ConstDefn Global
                    (Fix (TyConst (Fix (TyStd (L (AlexPn 6 1 7) IdStdType "int")))))
                    (L (AlexPn 10 1 11) IdVar "a")
                    (Fix (LiteralExpr Int (L (AlexPn 14 1 15) LitInteger "3"))))) -> True
                _ -> False

    describe "Bifunctor NodeF" $ do
        it "bimap maps lexemes and children" $ do
            let fl l = "l-" ++ l
            let fa a = "a-" ++ a
            let test n e = bimap fl fa n `shouldBe` e

            -- Preprocessor
            test (PreprocInclude "inc") (PreprocInclude "l-inc")
            test (PreprocDefine "def") (PreprocDefine "l-def")
            test (PreprocDefineConst "def" "const") (PreprocDefineConst "l-def" "a-const")
            test (PreprocDefineMacro "def" ["p1", "p2"] "body") (PreprocDefineMacro "l-def" ["a-p1", "a-p2"] "a-body")
            test (PreprocIf "cond" ["as"] "else") (PreprocIf "a-cond" ["a-as"] "a-else")
            test (PreprocIfdef "def" ["as"] "else") (PreprocIfdef "l-def" ["a-as"] "a-else")
            test (PreprocIfndef "def" ["as"] "else") (PreprocIfndef "l-def" ["a-as"] "a-else")
            test (PreprocElse ["as"]) (PreprocElse ["a-as"])
            test (PreprocElif "cond" ["as"] "else") (PreprocElif "a-cond" ["a-as"] "a-else")
            test (PreprocUndef "def") (PreprocUndef "l-def")
            test (PreprocDefined "def") (PreprocDefined "l-def")
            test (PreprocScopedDefine "a" ["as"] "a'") (PreprocScopedDefine "a-a" ["a-as"] "a-a'")
            test (MacroBodyStmt "a") (MacroBodyStmt "a-a")
            test (MacroBodyFunCall "a") (MacroBodyFunCall "a-a")
            test (MacroParam "l") (MacroParam "l-l")
            test (StaticAssert "a" "l") (StaticAssert "a-a" "l-l")

            -- Comments
            test (LicenseDecl "l" ["as"]) (LicenseDecl "l-l" ["a-as"])
            test (CopyrightDecl "l" (Just "ml") ["ls"]) (CopyrightDecl "l-l" (Just "l-ml") ["l-ls"])
            test (Comment Regular "l" ["ls"] "l'") (Comment Regular "l-l" ["l-ls"] "l-l'")
            test (CommentSection "a" ["as"] "a'") (CommentSection "a-a" ["a-as"] "a-a'")
            test (CommentSectionEnd "l") (CommentSectionEnd "l-l")
            test (Commented "a" "a'") (Commented "a-a" "a-a'")
            test (CommentInfo (Fix (DocWord "l"))) (CommentInfo (Fix (DocWord "l-l")))

            -- Namespace-like blocks
            test (ExternC ["as"]) (ExternC ["a-as"])
            test (Group ["as"]) (Group ["a-as"])

            -- Statements
            test (CompoundStmt ["as"]) (CompoundStmt ["a-as"])
            test Break Break
            test (Goto "l") (Goto "l-l")
            test Continue Continue
            test (Return (Just "ma")) (Return (Just "a-ma"))
            test (SwitchStmt "a" ["as"]) (SwitchStmt "a-a" ["a-as"])
            test (IfStmt "a" "a'" (Just "ma''")) (IfStmt "a-a" "a-a'" (Just "a-ma''"))
            test (ForStmt "a" "a'" "a''" "a'''") (ForStmt "a-a" "a-a'" "a-a''" "a-a'''")
            test (WhileStmt "a" "as") (WhileStmt "a-a" "a-as")
            test (DoWhileStmt "a" "a'") (DoWhileStmt "a-a" "a-a'")
            test (Case "a" "a'") (Case "a-a" "a-a'")
            test (Default "a") (Default "a-a")
            test (Label "l" "a") (Label "l-l" "a-a")
            test (ExprStmt "a") (ExprStmt "a-a")

            -- Variable declarations
            test (VLA "a" "l" "a'") (VLA "a-a" "l-l" "a-a'")
            test (VarDeclStmt "a" (Just "ma")) (VarDeclStmt "a-a" (Just "a-ma"))
            test (VarDecl "a" "l" ["as"]) (VarDecl "a-a" "l-l" ["a-as"])
            test (DeclSpecArray NullabilityUnspecified (Just "ma")) (DeclSpecArray NullabilityUnspecified (Just "a-ma"))

            -- Expressions
            test (InitialiserList ["as"]) (InitialiserList ["a-as"])
            test (UnaryExpr UopNot "a") (UnaryExpr UopNot "a-a")
            test (BinaryExpr "a" BopEq "a'") (BinaryExpr "a-a" BopEq "a-a'")
            test (TernaryExpr "a" "a'" "a''") (TernaryExpr "a-a" "a-a'" "a-a''")
            test (AssignExpr "a" AopEq "a'") (AssignExpr "a-a" AopEq "a-a'")
            test (ParenExpr "a") (ParenExpr "a-a")
            test (CastExpr "a" "a'") (CastExpr "a-a" "a-a'")
            test (CompoundLiteral "a" "a'") (CompoundLiteral "a-a" "a-a'")
            test (SizeofExpr "a") (SizeofExpr "a-a")
            test (SizeofType "a") (SizeofType "a-a")
            test (LiteralExpr Int "l") (LiteralExpr Int "l-l")
            test (VarExpr "l") (VarExpr "l-l")
            test (MemberAccess "a" "l") (MemberAccess "a-a" "l-l")
            test (PointerAccess "a" "l") (PointerAccess "a-a" "l-l")
            test (ArrayAccess "a" "a'") (ArrayAccess "a-a" "a-a'")
            test (FunctionCall "a" ["as"]) (FunctionCall "a-a" ["a-as"])
            test (CommentExpr "a" "a'") (CommentExpr "a-a" "a-a'")

            -- Type definitions
            test (EnumConsts (Just "ml") ["as"]) (EnumConsts (Just "l-ml") ["a-as"])
            test (EnumDecl "l" ["as"] "l'") (EnumDecl "l-l" ["a-as"] "l-l'")
            test (Enumerator "l" (Just "ma")) (Enumerator "l-l" (Just "a-ma"))
            test (AggregateDecl "a") (AggregateDecl "a-a")
            test (Typedef "a" "l") (Typedef "a-a" "l-l")
            test (TypedefFunction "a") (TypedefFunction "a-a")
            test (Struct "l" ["as"]) (Struct "l-l" ["a-as"])
            test (Union "l" ["as"]) (Union "l-l" ["a-as"])
            test (MemberDecl "a" (Just "ml")) (MemberDecl "a-a" (Just "l-ml"))
            test (TyBitwise "a") (TyBitwise "a-a")
            test (TyForce "a") (TyForce "a-a")
            test (TyConst "a") (TyConst "a-a")
            test (TyOwner "a") (TyOwner "a-a")
            test (TyNonnull "a") (TyNonnull "a-a")
            test (TyNullable "a") (TyNullable "a-a")
            test (TyPointer "a") (TyPointer "a-a")
            test (TyStruct "l") (TyStruct "l-l")
            test (TyUnion "l") (TyUnion "l-l")
            test (TyFunc "l") (TyFunc "l-l")
            test (TyStd "l") (TyStd "l-l")
            test (TyUserDefined "l") (TyUserDefined "l-l")

            -- Functions
            test (AttrPrintf "l" "l'" "a") (AttrPrintf "l-l" "l-l'" "a-a")
            test (FunctionDecl Global "a") (FunctionDecl Global "a-a")
            test (FunctionDefn Global "a" "a'") (FunctionDefn Global "a-a" "a-a'")
            test (FunctionPrototype "a" "l" ["as"]) (FunctionPrototype "a-a" "l-l" ["a-as"])
            test (CallbackDecl "l" "l'") (CallbackDecl "l-l" "l-l'")
            test Ellipsis Ellipsis
            test (NonNull ["ls"] ["ls'"] "a") (NonNull ["l-ls"] ["l-ls'"] "a-a")
            test (NonNullParam "a") (NonNullParam "a-a")
            test (NullableParam "a") (NullableParam "a-a")

            -- Constants
            test (ConstDecl "a" "l") (ConstDecl "a-a" "l-l")
            test (ConstDefn Global "a" "l" "a'") (ConstDefn Global "a-a" "l-l" "a-a'")

    describe "Bifunctor CommentF" $ do
        it "bimap maps lexemes and children" $ do
            let fl l = "l-" ++ l
            let fa a = "a-" ++ a
            let test n e = bimap fl fa n `shouldBe` e

            test (DocComment ["as"]) (DocComment ["a-as"])
            test DocAttention DocAttention
            test DocBrief DocBrief
            test DocDeprecated DocDeprecated
            test (DocExtends "l") (DocExtends "l-l")
            test DocFile DocFile
            test (DocImplements "l") (DocImplements "l-l")
            test DocNote DocNote
            test (DocParam (Just "ml") "l") (DocParam (Just "l-ml") "l-l")
            test DocReturn DocReturn
            test DocRetval DocRetval
            test (DocSection "l") (DocSection "l-l")
            test (DocSecurityRank "l" (Just "ml") "l'") (DocSecurityRank "l-l" (Just "l-ml") "l-l'")
            test (DocSee "l") (DocSee "l-l")
            test (DocSubsection "l") (DocSubsection "l-l")
            test DocPrivate DocPrivate
            test (DocLine ["as"]) (DocLine ["a-as"])
            test (DocCode "l" ["as"] "l'") (DocCode "l-l" ["a-as"] "l-l'")
            test (DocWord "l") (DocWord "l-l")
            test (DocRef "l") (DocRef "l-l")
            test (DocP "l") (DocP "l-l")

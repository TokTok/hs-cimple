{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
module Language.Cimple.ASTSpec where

import           Data.Fix                     (Fix (..), hoistFix)
import           Data.Functor.Classes         (Eq1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           Data.Functor.Compose         (Compose (..))
import           GHC.Generics                 (Generic, Generic1)
import           Test.Hspec                   (Spec, describe, it, shouldBe,
                                               shouldSatisfy)

import           Language.Cimple              (AlexPosn (..), Lexeme (..),
                                               LexemeClass (..),
                                               LiteralType (..), Node,
                                               NodeF (..), Scope (..))
import           Language.Cimple.IO           (parseText)

data AnnotF attr a = Annot { attr :: attr, unAnnot :: a }
    deriving (Functor, Generic, Generic1)
    deriving (Show1, Read1, Eq1) via FunctorClassesDefault (AnnotF attr)

type AnnotNode lexeme = Fix (AnnotF () `Compose` NodeF lexeme)

addAnnot :: Node lexeme -> AnnotNode lexeme
addAnnot = hoistFix $ Compose . Annot ()

removeAnnot :: AnnotNode lexeme -> Node lexeme
removeAnnot = hoistFix $ unAnnot . getCompose


spec :: Spec
spec = do
    describe "Node" $ do
        it "can be annotated" $ do
            let Right [ast] = parseText "const int a = 3;"
            addAnnot ast `shouldBe`
                Fix (Compose (Annot () (
                    ConstDefn Global
                        (Fix (Compose (Annot () (
                            TyStd (L (AlexPn 6 1 7) IdStdType "int")))))
                        (L (AlexPn 10 1 11) IdVar "a")
                        (Fix (Compose (Annot () (
                            LiteralExpr Int (L (AlexPn 14 1 15) LitInteger "3"))))))))

            removeAnnot (addAnnot ast) `shouldBe` ast

            addAnnot ast `shouldSatisfy` \case
                (removeAnnot -> Fix (ConstDefn Global
                    (Fix (TyStd (L (AlexPn 6 1 7) IdStdType "int")))
                    (L (AlexPn 10 1 11) IdVar "a")
                    (Fix (LiteralExpr Int (L (AlexPn 14 1 15) LitInteger "3"))))) -> True
                _ -> False

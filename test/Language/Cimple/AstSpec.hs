{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Language.Cimple.AstSpec where

import           Data.Fix             (Fix (..))
import           Data.Functor.Compose (Compose (..))
import           Test.Hspec           (Spec, describe, it, shouldBe,
                                       shouldSatisfy)

import           Language.Cimple      (AlexPosn (..), AnnotF (..), Lexeme (..),
                                       LexemeClass (..), LiteralType (..),
                                       NodeF (..), Scope (..), addAnnot,
                                       removeAnnot)
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

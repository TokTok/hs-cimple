{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Cimple.DescribeAstSpec where

import           Test.Hspec          (Spec, describe, it, shouldBe,
                                      shouldNotContain)

import qualified Data.List.Extra     as List
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Language.Cimple.IO  (parseExpr, parseStmt, parseText)
import           Language.CimpleSpec (sampleToken)
import           Test.QuickCheck     (Testable (property))


expected :: (Text -> Either String a) -> Text -> String
expected parse code =
    case parse code of
        Left err -> snd $ List.breakOn "expected " err
        Right _  -> ""


spec :: Spec
spec = do
    describe "error messages" $ do
        it "has useful suggestions" $ do
            parseText "int a() {}" `shouldBe` Left
                ":1:10: Parse error near right brace: \"}\"; expected statement or declaration"

            expected parseText "Beep Boop" `shouldBe`
                "expected variable name"

            expected parseText "const *a() {}" `shouldBe`
                "expected type specifier"

            expected parseText "int a() { int }" `shouldBe`
                "expected variable name"

        it "has suggestions for any sequence of tokens in top level" $ do
            property $ \tokens ->
                expected parseText (Text.intercalate " " (map sampleToken tokens)) `shouldNotContain`
                    "expected one of"

        it "has suggestions for any sequence of tokens in expressions" $ do
            property $ \tokens ->
                expected parseExpr (Text.intercalate " " (map sampleToken tokens)) `shouldNotContain`
                    "expected one of"

        it "has suggestions for any sequence of tokens in statements" $ do
            property $ \tokens ->
                expected parseStmt (Text.intercalate " " (map sampleToken tokens)) `shouldNotContain`
                    "expected one of"

        it "does not support multiple declarators per declaration" $ do
            let ast = parseText "int main() { int a, b; }"
            ast `shouldBe` Left
                ":1:19: Parse error near comma: \",\"; expected '=' or ';'"

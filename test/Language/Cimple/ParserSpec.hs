{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.ParserSpec where

import           Control.Monad       (unless, when)
import           Data.Fix            (Fix (..))
import           Data.List           (isInfixOf)
import qualified Data.List.Extra     as List
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Language.Cimple     (AlexPosn (..), Lexeme (..),
                                      LexemeClass (..), Node, NodeF (..),
                                      Scope (..), UnaryOp (..),
                                      formatParseError, preprocessorEnabled)
import           Language.Cimple.IO  (parseExpr, parseStmt, parseText)
import           Language.CimpleSpec (sampleToken)
import           Test.Hspec          (Spec, describe, expectationFailure, it,
                                      pendingWith, shouldBe, shouldSatisfy)
import           Test.QuickCheck     (Testable (property))


isRight1 :: Either a [b] -> Bool
isRight1 (Right [_]) = True
isRight1 _           = False


parseLines :: [Text] -> Either String [Node (Lexeme Text)]
parseLines = parseText . Text.unlines


spec :: Spec
spec = do
    describe "C parsing" $ do
        it "should parse a simple function" $ do
            let ast = parseText "int a(void) { return 3; }"
            ast `shouldSatisfy` isRight1

        it "should parse a const function declaration" $ do
            let ast = parseText "const int f(void);"
            ast `shouldSatisfy` isRight1

        it "should parse dereferencing a function pointer" $ do
            let ast = parseText "void f() { struct My_Struct s_var = *get_s(1); }"
            ast `shouldSatisfy` isRight1

        it "should parse *f(x) as *(f(x))" $ do
            let ast = parseLines ["void g() { int x = *f(1); }"]
            case ast of
                Right [Fix (FunctionDefn _ _ (Fix (CompoundStmt [Fix (VarDeclStmt _ (Just (Fix (UnaryExpr UopDeref (Fix (FunctionCall (Fix (VarExpr (L _ _ "f"))) [_]))))))])))] -> return ()
                _ -> expectationFailure $ "Unexpected AST: " ++ show ast

        it "should parse per-param non_null and nullable annotations" $ do
            let ast = parseText "int a(char *_Nonnull p, int *_Nullable q);"
            ast `shouldSatisfy` isRight1

        it "should parse a type declaration" $ do
            let ast = parseText "typedef struct Foo { int x; } Foo;"
            ast `shouldSatisfy` isRight1

        it "should parse a struct with bit fields" $ do
            let ast = parseText "typedef struct Foo { int x : 123; } Foo;"
            ast `shouldSatisfy` isRight1

        it "should parse a struct with preprocessor conditionals" $ do
            let ast = parseText $ Text.unlines
                    [ "struct Foo {"
                    , "  int x;"
                    , "#ifndef HAVE_FOO_BAR"
                    , "  int foo_bar;"
                    , "#endif /* HAVE_FOO_BAR */"
                    , "  int y;"
                    , "};"
                    ]
            ast `shouldSatisfy` isRight1

        it "should parse a typedef with an array" $ do
            let ast = parseText "typedef uint8_t Public_Key[32];"
            ast `shouldSatisfy` isRight1

        it "should parse #ifdef/#elif" $ do
            let ast = parseText $ Text.unlines
                    [ "struct Foo {"
                    , "  int x;"
                    , "#ifdef HAVE_FOO_BAR"
                    , "  int foo_bar;"
                    , "#elif defined(HAVE_BAR_FOO)"
                    , "  int bar_foo;"
                    , "#endif /* HAVE_FOO_BAR */"
                    , "  int y;"
                    , "};"
                    ]
            ast `shouldSatisfy` isRight1

        it "should parse #ifndef/#elif" $ do
            let ast = parseText $ Text.unlines
                    [ "struct Foo {"
                    , "  int x;"
                    , "#ifndef HAVE_FOO_BAR"
                    , "  int foo_bar;"
                    , "#elif defined(HAVE_BAR_FOO)"
                    , "  int bar_foo;"
                    , "#endif /* HAVE_FOO_BAR */"
                    , "  int y;"
                    , "};"
                    ]
            ast `shouldSatisfy` isRight1

        it "should parse LOGGER_INFO as a constant" $ do
            let ast = parseText "void f() { int x = LOGGER_INFO; }"
            ast `shouldSatisfy` isRight1

        it "should parse IP_ADAPTER_INFO as a type" $ do
            let ast = parseText "void f() { IP_ADAPTER_INFO *x; }"
            ast `shouldSatisfy` isRight1

        it "should parse LARGE_INTEGER as a type" $ do
            let ast = parseText "void f() { LARGE_INTEGER x; }"
            ast `shouldSatisfy` isRight1

        it "should parse POLLIN as a constant" $ do
            let ast = parseText "void f() { int x = POLLIN; }"
            ast `shouldSatisfy` isRight1

        it "should parse WSAEWOULDBLOCK as a constant" $ do
            let ast = parseText "void f() { int x = WSAEWOULDBLOCK; }"
            ast `shouldSatisfy` isRight1

        it "should parse WSADATA as a type" $ do
            let ast = parseText "void f() { WSADATA wsaData; }"
            ast `shouldSatisfy` isRight1

        it "should parse DHT as a type" $ do
            let ast = parseText "void f() { DHT *dht; }"
            ast `shouldSatisfy` isRight1

        it "should parse _WIN32 as a constant" $ do
            let ast = parseText "void f() { int x = _WIN32; }"
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

    if preprocessorEnabled
    then do
        describe "error messages" $ do
            let expected parse code =
                    case parse code of
                        Left err -> snd $ List.breakOn "expected " err
                        Right _  -> ""

            when preprocessorEnabled $ do
                it "includes context breadcrumbs" $ do
                    parseText "int a() { if (1) x = 1; }" `shouldBe` Left
                        ":1:18: Parse error while parsing function near variable name: \"x\"; expected return or '{'"

                it "has useful suggestions" $ do
                    parseText "int a() {}" `shouldBe` Left
                        ":1:10: Parse error while parsing function near right brace: \"}\"; expected statement or declaration"

            it "has useful suggestions (no context)" $ do
                expected parseText "Beep Boop" `shouldBe`
                    "expected variable name\nHint: Found type name: \"Boop\", but here we expected a variable name."

                expected parseText "const *a() {}" `shouldBe`
                    "expected type specifier"

                expected parseText "int a() { int }" `shouldBe`
                    "expected variable name"

                expected parseStmt "(int){" `shouldBe`
                    "expected constant or literal"

            it "has suggestions for any sequence of tokens in top level" $ do
                property $ \tokens -> do
                    let msg = expected parseText (Text.intercalate " " (map sampleToken tokens))
                    unless ("\"ifndefDefine\"" `isInfixOf` msg) $
                        msg `shouldSatisfy` (not . List.isInfixOf "expected one of")

            it "has suggestions for any sequence of tokens in expressions" $ do
                property $ \tokens ->
                    expected parseExpr (Text.intercalate " " (map sampleToken tokens)) `shouldSatisfy`
                        (not . List.isInfixOf "expected one of")

            it "has suggestions for any sequence of tokens in statements" $ do
                property $ \tokens ->
                    expected parseStmt (Text.intercalate " " (map sampleToken tokens)) `shouldSatisfy`
                        (not . List.isInfixOf "expected one of")

            it "does not support multiple declarators per declaration" $ do
                let ast = parseText "int main() { int a, b; }"
                ast `shouldBe` Left
                    ":1:19: Parse error while parsing function > variable declaration near comma: \",\"; expected '=' or ';'"

        describe "contextual error messages" $ do
            it "reports errors in struct definitions" $ do
                parseText "struct My_Struct { int a }" `shouldBe` Left
                    ":1:26: Parse error while parsing struct/union definition type name: \"My_Struct\" near right brace: \"}\"; expected ':' or ';'"

            it "reports errors in enum definitions" $ do
                parseText "enum My_Enum { A, B= }" `shouldBe` Left
                    ":1:22: Parse error while parsing enum definition type name: \"My_Enum\" near right brace: \"}\"; expected preprocessor constant expression"

            it "reports errors in for loop headers" $ do
                parseText "void f() { for (int i=0; i<10) {} }" `shouldBe` Left
                    ":1:30: Parse error while parsing function near right parenthesis: \")\"; expected operator or end of statement"

            it "reports errors in nested blocks" $ do
                parseText "void f() { { int x = 1 } }" `shouldBe` Left
                    ":1:24: Parse error while parsing function > variable declaration near right brace: \"}\"; expected ';'"

            it "reports missing comment after #endif" $ do
                parseText "#ifdef A\nvoid f() { return; }\n#endif" `shouldBe` Left
                    ":3:1: Parse error while parsing #endif near end-of-file: \"\"; expected a comment\nHint: In Cimple, every #endif must be followed by a comment indicating what it closes (e.g. '#endif /* FLAG */')."

            it "includes captured lexemes in breadcrumbs" $ do
                pendingWith "no function name tracking yet"
                parseText "void my_func() { int x = 1 }" `shouldBe` Left
                    ":1:28: Parse error while parsing function variable name: \"my_func\" > variable declaration near right brace: \"}\"; expected ';'"

            it "reports errors in nested blocks with full context" $ do
                pendingWith "no function name tracking yet"
                parseText "void f() { { int x = 1; if (1) { y } } }" `shouldBe` Left
                    ":1:35: Parse error while parsing function near variable name: \"y\"; expected return or '{'"

            it "uses context location for EOF errors in functions" $ do
                pendingWith "no function name tracking yet"
                parseText "void unclosed_func(int x) {" `shouldBe` Left
                    ":1:6: Parse error while parsing function variable name: \"unclosed_func\" near end-of-file: \"\"; expected statement or declaration\nHint: Reached a terminator before finding the expected closing '}'."

            it "reports errors in function calls" $ do
                parseText "void f() { g(a, ); }" `shouldBe` Left
                    ":1:17: Parse error while parsing function near right parenthesis: \")\"; expected expression"

            it "reports errors in preprocessor blocks" $ do
                parseText "#if 1\nvoid f() {;\n#else\nint b\n#endif" `shouldBe` Left
                    ":2:11: Parse error while parsing function near end of statement semicolon: \";\"; expected statement or declaration"

            it "reports errors in macro bodies" $ do
                -- This is a semantic check in Parser.y, not a standard parse error.
                parseText "#define M(x) do { x = 1; } while (1)" `shouldBe` Left
                    "L (AlexPn 34 1 35) LitInteger \"1\": macro do-while body must end in 'while (0)'"

        describe "generic hints" $ do
            it "detects unclosed parenthesis in function calls" $ do
                -- In a function call, a semicolon is a hard terminator.
                -- The parser expects more arguments (',') or the end of the list (')').
                parseText "void f() { g(1; }" `shouldBe` Left
                    ":1:15: Parse error while parsing function near end of statement semicolon: \";\"; expected ',' or ')'\nHint: Reached a terminator before finding the expected closing ')'."

            it "detects unclosed brackets in array access" $ do
                parseExpr "a[1;" `shouldBe` Left
                    ":1:4: Parse error near end of statement semicolon: \";\"; expected operator\nHint: Reached a terminator before finding the expected closing ']'."

            it "detects unclosed braces in struct definitions" $ do
                parseText "struct My_Struct { int a; " `shouldBe` Left
                    ":1:8: Parse error while parsing struct/union definition type name: \"My_Struct\" near end-of-file: \"\"; expected '}'\nHint: Reached a terminator before finding the expected closing '}'."

            it "reports identifier category mismatch" $ do
                parseText "struct S { int a; };" `shouldBe` Left
                    ":1:8: Parse error near constant name: \"S\"; expected type name\nHint: Found constant name: \"S\", but here we expected a type name."

            it "reports reserved keyword used as identifier" $ do
                parseText "int if = 1;" `shouldBe` Left
                    ":1:5: Parse error near \"if\"; expected variable name\nHint: \"if\" is a reserved keyword and cannot be used as an identifier."

    else do
        describe "error messages" $ do
            it "does not support multiple declarators per declaration" $ do
                let ast = parseText "int main() { int a, b; }"
                ast `shouldBe` Left
                    ":1:19: Parse error near comma: \",\"; expected '=' or ';'"

module Language.Cimple.PrettySpec where

import           Test.Hspec                   (Spec, describe, it, shouldBe)

import qualified Data.Text                    as Text
import           Language.Cimple.IO           (parseText)
import           Language.Cimple.Pretty       (ppTranslationUnit)
import           Text.PrettyPrint.ANSI.Leijen (displayS, renderCompact)

getRight :: Either String a -> a
getRight (Left  err) = error err
getRight (Right ok ) = ok

pretty :: String -> String
pretty =
    show
    . ppTranslationUnit
    . getRight
    . parseText
    . Text.pack

compact :: String -> String
compact =
    flip displayS ""
    . renderCompact
    . ppTranslationUnit
    . getRight
    . parseText
    . Text.pack


spec :: Spec
spec = do
    describe "renderPretty" $ do
        it "pretty-prints a simple C function" $ do
            let pp = pretty "int a(void) { return 3; }"
            pp `shouldBe` unlines
                [ "int a(void) {"
                , "  return 3;"
                , "}"
                ]

        it "pretty-prints a typedef struct" $ do
            let pp = pretty "typedef struct Foo { int32_t x; } Foo;"
            pp `shouldBe` unlines
                [ "typedef struct Foo {"
                , "  int32_t x;"
                , "} Foo;"
                ]

    describe "renderCompact" $ do
        it "pretty-prints a simple C function" $ do
            let pp = compact "int a(void) { return 3; }"
            pp `shouldBe` unlines
                [ "int a(void) {"
                , "return 3;"
                , "}"
                ]

        it "pretty-prints a typedef struct" $ do
            let pp = compact "typedef struct Foo { int32_t x; } Foo;"
            pp `shouldBe` unlines
                [ "typedef struct Foo {"
                , "int32_t x;"
                , "} Foo;"
                ]

        it "respects newlines at end of comments" $ do
            compact "/* foo bar */" `shouldBe` "/* foo bar */\n"
            compact "/* foo bar\n */" `shouldBe` "/* foo bar\n*/\n"

        it "respects comment styles" $ do
            compact "/* foo bar */" `shouldBe` "/* foo bar */\n"
            compact "/** foo bar */ int a(void);" `shouldBe` "/** foo bar */\n\nint a(void);\n"
            compact "/*** foo bar */" `shouldBe` "/*** foo bar */\n"
            compact "/**** foo bar */" `shouldBe` "/*** foo bar */\n"

        it "supports punctuation in comments" $ do
            compact "/* foo.bar,baz-blep */"
                `shouldBe` "/* foo . bar , baz - blep */\n"
            compact "/* foo? */" `shouldBe` "/* foo ? */\n"
            compact "/* 123 - 456 */" `shouldBe` "/* 123 - 456 */\n"
            compact "/* - 3 */" `shouldBe` "/* - 3 */\n"
            compact "/* a-b */" `shouldBe` "/* a - b */\n"

        it "formats pointer types with east-const" $ do
            compact "void foo(const int *a);"
                `shouldBe` "void foo(int const* a);\n"
            compact "void foo(int const *a);"
                `shouldBe` "void foo(int const* a);\n"

        it "formats expressions and statements" $ do
            compact "void foo() { return 1+2*3/4; }"
                `shouldBe` "void foo() {\nreturn 1 + 2 * 3 / 4;\n}\n"
            compact "void foo() { a = ~b << !c; }"
                `shouldBe` "void foo() {\na = ~b << !c;\n}\n"
            compact "void foo() { int a[] = {1,2,3}; }"
                `shouldBe` "void foo() {\nint a[] = { 1, 2, 3 };\n}\n"

        it "supports variadic functions" $ do
            compact "void foo(int a, ...);"
                `shouldBe` "void foo(int a, ...);\n"
            compact "void foo(int a, const char *msg, ...);"
                `shouldBe` "void foo(int a, char const* msg, ...);\n"

        it "supports C preprocessor directives" $ do
            compact "#define XYZZY 123\n"
                `shouldBe` "#define XYZZY 123\n"
            compact "#include <tox/tox.h>\n"
                `shouldBe` "#include <tox/tox.h>\n"

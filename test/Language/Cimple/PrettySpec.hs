{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.PrettySpec where

import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import           Language.Cimple               (Lexeme, Node)
import           Language.Cimple.IO            (parseText)
import           Language.Cimple.Pretty        (plain, ppTranslationUnit,
                                                showNode)
import           Prettyprinter                 (SimpleDocStream, layoutCompact)
import           Prettyprinter.Render.Terminal (AnsiStyle, renderLazy)

getRight :: Either String a -> a
getRight (Left  err) = error err
getRight (Right ok ) = ok

pretty :: String -> String
pretty =
    show
    . plain
    . ppTranslationUnit
    . mustParse

compact :: String -> String
compact =
    flip displayS ""
    . layoutCompact
    . plain
    . ppTranslationUnit
    . mustParse

mustParse :: String -> [Node (Lexeme Text)]
mustParse =
    getRight
    . parseText
    . Text.pack

displayS :: SimpleDocStream AnsiStyle -> ShowS
displayS sdoc =
    let rendered = renderLazy sdoc
    in (TL.unpack rendered ++)


spec :: Spec
spec = do
    -- implementation: Pretty.hs (ppComment).
    describe "Regular comment pretty-printing" $ do
        it "should pretty-print a doc comment" $
            pretty (unlines
                [ "/*"
                , " * @brief hello world."
                , " *"
                , " * This is cool stuff."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/*"
                , " * @brief hello world."
                , " *"
                , " * This is cool stuff."
                , " */"
                , ""
                , "const int abc = 3;"
                ]

    describe "showNode" $ do
        it "prints code with syntax highlighting" $ do
            let pp = showNode $ head $ mustParse "int a(void) { return 3; }"
            pp <> "\n" `shouldBe` Text.unlines
                [ "\ESC[0;32mint\ESC[0m a(\ESC[0;32mvoid\ESC[0m) {"
                , "  \ESC[0;31mreturn\ESC[0m \ESC[0;31m3\ESC[0m;"
                , "}"
                ]

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

        it "pretty-prints ifdef'd struct members in the correct order" $
            compact (unlines
                [ "typedef struct Foo {"
                , "  int32_t a;"
                , "#ifdef ENABLE_XY"
                , "  int32_t x;"
                , "  int32_t y;"
                , "#endif /* ENABLE_XY */"
                , "} Foo;"
                ])
            `shouldBe` unlines
                [ "typedef struct Foo {"
                , "int32_t a;"
                , "#ifdef ENABLE_XY"
                , "int32_t x;"
                , ""
                , "int32_t y;"
                , "#endif  /* ENABLE_XY */"
                , "} Foo;"
                ]

        it "respects newlines at end of comments" $ do
            compact "/* foo bar */" `shouldBe` "/* foo bar */\n"
            compact "/* foo bar\n */" `shouldBe` "/* foo bar\n*/\n"

        it "respects comment styles" $ do
            compact "/* foo bar */" `shouldBe` "/* foo bar */\n"
            compact "/*** foo bar */" `shouldBe` "/*** foo bar */\n"
            compact "/**** foo bar */" `shouldBe` "/*** foo bar */\n"

--      it "properly formats doxygen comments" $ do
--          compact "/** foo bar */ int a(void);" `shouldBe` "/** foo bar */\n\nint a(void);\n"

        it "supports punctuation in comments" $ do
            compact "/* foo.bar,baz-blep */"
                `shouldBe` "/* foo.bar,baz-blep */\n"
            compact "/* foo? */" `shouldBe` "/* foo? */\n"
            compact "/* 123 - 456 */" `shouldBe` "/* 123 - 456 */\n"
            compact "/* - 3 */" `shouldBe` "/* - 3 */\n"
            compact "/* a-b */" `shouldBe` "/* a-b */\n"

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
            compact "#ifndef XYZZY\n#define XYZZY 123\n#endif /* XYZZY */\n"
                `shouldBe` "#ifndef XYZZY\n#define XYZZY 123\n#endif  /* XYZZY */\n"
            compact "#include \"tox.h\"\n"
                `shouldBe` "#include \"tox.h\"\n"

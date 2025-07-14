{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.PrettyCommentSpec where

import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import           Language.Cimple               (Lexeme, Node)
import           Language.Cimple.IO            (parseText)
import           Language.Cimple.Pretty        (plain, ppTranslationUnit,
                                                renderSmart)
import           Prettyprinter                 (SimpleDocStream, layoutCompact)
import           Prettyprinter.Render.Terminal (AnsiStyle, renderLazy)

getRight :: Either String a -> a
getRight (Left  err) = error err
getRight (Right ok ) = ok

compact :: String -> String
compact =
    flip displayS ""
    . renderSmart 1 120
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
    -- implementation: PrettyComment.hs (ppCommentInfo).
    describe "Doxygen comment pretty-printing" $ do
        it "should pretty-print a doc comment" $
            compact (unlines
                [ "/**"
                , " * @brief hello world."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @brief hello world."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a doc comment with a paragraph" $
            compact (unlines
                [ "/**"
                , " * @brief hello world."
                , " *"
                , " * There's more to say about today."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @brief hello world."
                , " *"
                , " * There's more to say about today."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a @param" $
            compact (unlines
                [ "/**"
                , " * @param p1 hello world."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @param p1 hello world."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a @param with [in] attribute" $
            compact (unlines
                [ "/**"
                , " * @param[in] p1 hello world."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @param[in] p1 hello world."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a @return" $
            compact (unlines
                [ "/**"
                , " * @return hello world."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @return hello world."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a @retval" $
            compact (unlines
                [ "/**"
                , " * @retval 0 Success."
                , " * @retval 1 Failure."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @retval 0 Success."
                , " * @retval 1 Failure."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a bullet list" $
            compact (unlines
                [ "/**"
                , " * - item 1"
                , " * - item 2"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * - item 1"
                , " * - item 2"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a nested bullet list" $
            compact (unlines
                [ "/**"
                , " * - item 1"
                , " *   - nested item 1"
                , " * - item 2"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * - item 1"
                , " *   - nested item 1"
                , " * - item 2"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a numbered list" $
            compact (unlines
                [ "/**"
                , " * 1. item 1"
                , " * 2. item 2"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * 1. item 1"
                , " * 2. item 2"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a code block" $
            compact (unlines
                [ "/**"
                , " * @code"
                , " * int x = 5,"
                , " *     y = 3;"
                , " * @endcode"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @code"
                , " * int x = 5,"
                , " *     y = 3;"
                , " * @endcode"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @deprecated" $
            compact (unlines
                [ "/**"
                , " * @deprecated Use new_function instead."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @deprecated Use new_function instead."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @see" $
            compact (unlines
                [ "/**"
                , " * @see other_function for more details."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @see other_function for more details."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @attention" $
            compact (unlines
                [ "/**"
                , " * @attention This is important."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @attention This is important."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @note" $
            compact (unlines
                [ "/**"
                , " * @note This is a note."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @note This is a note."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @private" $
            compact (unlines
                [ "/**"
                , " * @private"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @private"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @extends" $
            compact (unlines
                [ "/**"
                , " * @extends BaseClass"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @extends BaseClass"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @implements" $
            compact (unlines
                [ "/**"
                , " * @implements Interface"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @implements Interface"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @security_rank" $
            compact (unlines
                [ "/**"
                , " * @security_rank(high, 100)"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @security_rank(high, 100)"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print a complex comment" $
            compact (unlines
                [ "/**"
                , " * @brief A brief description."
                , " *"
                , " * A more detailed description."
                , " * - list item 1"
                , " * - list item 2"
                , " *"
                , " * @param p1 Description for p1."
                , " * @return Description for return."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @brief A brief description."
                , " *"
                , " * A more detailed description."
                , " * - list item 1"
                , " * - list item 2"
                , " *"
                , " * @param p1 Description for p1."
                , " * @return Description for return."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print words with operators" $
            compact (unlines
                [ "/**"
                , " * check if x == 5"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * check if x == 5"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @ref" $
            compact (unlines
                [ "/**"
                , " * see @ref my_ref"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * see @ref my_ref"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @p" $
            compact (unlines
                [ "/**"
                , " * @p my_p"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @p my_p"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @file" $
            compact (unlines
                [ "/**"
                , " * @file"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @file"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @section" $
            compact (unlines
                [ "/**"
                , " * @section sec_id sec_title"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @section sec_id sec_title"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print @subsection" $
            compact (unlines
                [ "/**"
                , " * @subsection subsec_id subsec_title"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * @subsection subsec_id subsec_title"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print words with colon" $
            compact (unlines
                [ "/**"
                , " * Note:"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * Note:"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print words with parens" $
            compact (unlines
                [ "/**"
                , " * (Note)"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * (Note)"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print words with parens including long sentences going to the next line" $
            compact (unlines
                [ "/**"
                , " * This is a comment (and there is more"
                , " * to say about this)."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * This is a comment (and there is more"
                , " * to say about this)."
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print words with operators" $
            compact (unlines
                [ "/**"
                , " * a + b = c"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * a + b = c"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print words with assignment" $
            compact (unlines
                [ "/**"
                , " * x = 5"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * x = 5"
                , " */"
                , "const int abc = 3;"
                ]

        it "should pretty-print sentences with various punctuation" $
            compact (unlines
                [ "/**"
                , " * Is this a question?"
                , " * This is exciting!"
                , " * This is a clause;"
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * Is this a question?"
                , " * This is exciting!"
                , " * This is a clause;"
                , " */"
                , "const int abc = 3;"
                ]

        it "should allow numbers in the text" $
            compact (unlines
                [ "/**"
                , " * I have 3 things on my mind."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * I have 3 things on my mind."
                , " */"
                , "const int abc = 3;"
                ]

        it "should allow numbers at the start of the text" $
            compact (unlines
                [ "/**"
                , " * I have 3 things on my mind. You have"
                , " * 20 things on your mind. We are not the same."
                , " */"
                , "const int my_mind = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * I have 3 things on my mind. You have"
                , " * 20 things on your mind. We are not the same."
                , " */"
                , "const int my_mind = 3;"
                ]

        it "should allow numbers at the end of the sentence at the start of a line" $
            compact (unlines
                [ "/**"
                , " * I have 3 things on my mind. The number of things on your mind is"
                , " * 20."
                , " */"
                , "const int my_mind = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * I have 3 things on my mind. The number of things on your mind is"
                , " * 20."
                , " */"
                , "const int my_mind = 3;"
                ]

        it "should treat numbers like words and be able to continue normally" $
            compact (unlines
                [ "/**"
                , " * Here is a bunch of text and then on the next line there is"
                , " * 1, maybe more or less, and we can talk a lot about"
                , " * 2022. This is the next sentence."
                , " */"
                , "const int my_mind = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * Here is a bunch of text and then on the next line there is"
                , " * 1, maybe more or less, and we can talk a lot about"
                , " * 2022. This is the next sentence."
                , " */"
                , "const int my_mind = 3;"
                ]

        it "should print numbered lists as is" $
            compact (unlines
                [ "/**"
                , " * Here is a bunch of text and now follows a list."
                , " *"
                , " * 1. This is the first item."
                , " * 2. This is the second item."
                , " *"
                , " * Here is some more text not part of the list items."
                , " */"
                , "const int my_mind = 3;"
                ])
            `shouldBe` unlines
                [ "/**"
                , " * Here is a bunch of text and now follows a list."
                , " *"
                , " * 1. This is the first item."
                , " * 2. This is the second item."
                , " *"
                , " * Here is some more text not part of the list items."
                , " */"
                , "const int my_mind = 3;"
                ]

        it "should pretty-print a doc comment with content on the first line" $
            compact (unlines
                [ "/** @file test.c"
                , " * @brief hello world."
                , " */"
                , "const int abc = 3;"
                ])
            `shouldBe` unlines
                [ "/** @file test.c"
                , " * @brief hello world."
                , " */"
                , "const int abc = 3;"
                ]

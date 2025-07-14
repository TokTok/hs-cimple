{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.CommentParserSpec where

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Test.Hspec         (Spec, describe, it, shouldSatisfy)

import           Language.Cimple    (Lexeme, Node)
import           Language.Cimple.IO (parseText)


isRight1 :: Either a [b] -> Bool
isRight1 (Right [_]) = True
isRight1 _           = False


parseLines :: [Text] -> Either String [Node (Lexeme Text)]
parseLines = parseText . Text.unlines


spec :: Spec
spec = do
    describe "Doxygen comment parsing" $ do
        it "should parse a doc comment" $
            parseLines
                [ "/**"
                , " * @brief hello world."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a doc comment with a number at the end" $
            parseLines
                [ "/**"
                , " * @brief hello world."
                , " *"
                , " * There's more to say about today."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a @param" $
            parseLines
                [ "/**"
                , " * @param p1 hello world."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a @param with [in] attribute" $
            parseLines
                [ "/**"
                , " * @param[in] p1 hello world."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a @return" $
            parseLines
                [ "/**"
                , " * @return hello world."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a @retval" $
            parseLines
                [ "/**"
                , " * @retval 0 Success."
                , " * @retval 1 Failure."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a bullet list" $
            parseLines
                [ "/**"
                , " * - item 1"
                , " * - item 2"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a nested bullet list" $
            parseLines
                [ "/**"
                , " * - item 1"
                , " *   - nested item 1"
                , " * - item 2"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a numbered list" $
            parseLines
                [ "/**"
                , " * 1. item 1"
                , " * 2. item 2"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a numbered list with continuation" $
            parseLines
                [ "/**"
                , " * 1. item 1"
                , " *    continued."
                , " * 2. item 2"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a code block" $
            parseLines
                [ "/**"
                , " * @code"
                , " * int x = 5;"
                , " * @endcode"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @deprecated" $
            parseLines
                [ "/**"
                , " * @deprecated Use new_function instead."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @see" $
            parseLines
                [ "/**"
                , " * @see other_function for more details."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @attention" $
            parseLines
                [ "/**"
                , " * @attention This is important."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @note" $
            parseLines
                [ "/**"
                , " * @note This is a note."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @private" $
            parseLines
                [ "/**"
                , " * @private"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @extends" $
            parseLines
                [ "/**"
                , " * @extends BaseClass"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @implements" $
            parseLines
                [ "/**"
                , " * @implements Interface"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @security_rank" $
            parseLines
                [ "/**"
                , " * @security_rank(high, 100)"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse a complex comment" $
            parseLines
                [ "/**"
                , " * @brief A brief description."
                , " *"
                , " * A more detailed description."
                , " * - list item 1"
                , " * - list item 2"
                , " * @param p1 Description for p1."
                , " * @return Description for return."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse words with operators" $
            parseLines
                [ "/**"
                , " * check if x == 5"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @ref" $
            parseLines
                [ "/**"
                , " * see @ref my_ref"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @p" $
            parseLines
                [ "/**"
                , " * @p my_p"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @file" $
            parseLines
                [ "/**"
                , " * @file"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @section" $
            parseLines
                [ "/**"
                , " * @section sec_id sec_title"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse @subsection" $
            parseLines
                [ "/**"
                , " * @subsection subsec_id subsec_title"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse words with colon" $
            parseLines
                [ "/**"
                , " * Note:"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse words with parens" $
            parseLines
                [ "/**"
                , " * (Note)"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse words with parens including long sentences going to the next line" $
            parseLines
                [ "/**"
                , " * This is a comment (and there is more"
                , " * to say about this)."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse words with operators" $
            parseLines
                [ "/**"
                , " * a + b = c"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse words with assignment" $
            parseLines
                [ "/**"
                , " * x = 5"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should parse sentences with various punctuation" $
            parseLines
                [ "/**"
                , " * Is this a question?"
                , " * This is exciting!"
                , " * This is a clause;"
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should allow numbers in the text" $
            parseLines
                [ "/**"
                , " * I have 3 things on my mind."
                , " */"
                , "const int abc = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should allow numbers at the start of the text" $
            parseLines
                [ "/**"
                , " * I have 3 things on my mind. You have"
                , " * 20 things on your mind. We are not the same."
                , " */"
                , "const int my_mind = 3;"
                ]
            `shouldSatisfy` isRight1

        it "should allow numbers at the end of the sentence at the start of a line" $
            parseLines
                [ "/**"
                , " * I have 3 things on my mind. The number of things on your mind is"
                , " * 20."
                , " */"
                , "const int my_mind = 3;"
                ]
            `shouldSatisfy` isRight1

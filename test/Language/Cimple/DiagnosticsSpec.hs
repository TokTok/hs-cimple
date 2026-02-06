{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.DiagnosticsSpec where

import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           GHC.Stack                   (HasCallStack)
import           Language.Cimple             (AlexPosn (..), BinaryOp (..),
                                              Lexeme (..), LexemeClass (..),
                                              NodeF (..))
import           Language.Cimple.Diagnostics
import           Prettyprinter               (Doc, LayoutOptions (..),
                                              PageWidth (..),
                                              defaultLayoutOptions,
                                              layoutPretty, vsep)
import           Prettyprinter.Render.Text   (renderStrict)
import           Test.Hspec

spec :: Spec
spec = do
    describe "nodePosAndLen" $ do
        it "calculates position and length of a simple node" $ do
            let lexeme = L (AlexPn 10 2 5) IdVar "var"
            let node = Fix (VarExpr lexeme)
            let (pos, len) = nodePosAndLen "test.c" node
            pos `shouldBe` CimplePos "test.c" 2 5
            len `shouldBe` 3

        it "calculates position and length of a complex node" $ do
            let l1 = L (AlexPn 10 2 5) IdVar "x"
            -- l2 (operator) is skipped in flattening if not in the node structure as a child
            let l3 = L (AlexPn 30 2 25) IdVar "y"
            -- FunctionCall contains the function name and args, so it covers l1 to l3
            let node = Fix (FunctionCall (Fix (VarExpr l1)) [Fix (VarExpr l3)])
            let (pos, len) = nodePosAndLen "test.c" node
            pos `shouldBe` CimplePos "test.c" 2 5
            -- Length is from start of l1 (10) to end of l3 (30 + 1). 31 - 10 = 21.
            len `shouldBe` 21

    describe "Diagnostics State" $ do
        it "warn collects diagnostics" $ do
            let lexeme = L (AlexPn 0 1 1) IdVar ("foo" :: Text)
            let action = warn "test.c" lexeme "warning message"
            let diags = State.execState action [] :: [Text]
            diags `shouldBe` ["test.c:1: warning message"]

        it "warnRich collects rich diagnostics" $ do
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 1
                    , diagLen = 5
                    , diagLevel = WarningLevel
                    , diagMsg = "warning message"
                    , diagFlag = Just "some-flag"
                    , diagSpans = []
                    , diagFooter = []
                    }
            let action = warnRich diag
            let diags = State.execState action []
            case diags of
                [d] -> do
                    diagLevel d `shouldBe` WarningLevel
                    diagFlag d `shouldBe` Just "some-flag"
                _ -> expectationFailure "Expected exactly one diagnostic"

    describe "renderPure" $ do
        it "renders different levels" $ do
            let mkDiag lvl msg = Diagnostic
                    { diagPos = CimplePos "test.c" 1 1
                    , diagLen = 1
                    , diagLevel = lvl
                    , diagMsg = msg
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo mempty [mkDiag WarningLevel "warn"]
                [ "warning: warn"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                ]
            shouldRenderTo mempty [mkDiag NoteLevel "note"]
                [ "note: note"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                ]
            shouldRenderTo mempty [mkDiag HelpLevel "help"]
                [ "help: help"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                ]

        it "renders flags" $ do
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 1
                    , diagLen = 1
                    , diagLevel = WarningLevel
                    , diagMsg = "some warning"
                    , diagFlag = Just "my-check"
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo mempty [diag]
                [ "warning: some warning [-Wmy-check]"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                ]

        it "renders multiline messages" $ do
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 1
                    , diagLen = 1
                    , diagLevel = ErrorLevel
                    , diagMsg = "line 1\nline 2"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo mempty [diag]
                [ "error: line 1"
                , "       line 2"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                ]

        it "renders line numbers > 1000 correctly" $ do
            let dummyLines = replicate 999 "..." ++ ["target line"]
            let cache = Map.fromList [("test.c", dummyLines)]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1000 1
                    , diagLen = 6
                    , diagLevel = ErrorLevel
                    , diagMsg = "msg"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: msg"
                , "   --> test.c:1000:1"
                , "    |"
                , "1000| target line"
                , "    | ^^^^^^"
                ]

        it "renders line numbers > 10000 correctly" $ do
            let dummyLines = replicate 9999 "..." ++ ["target line"]
            let cache = Map.fromList [("test.c", dummyLines)]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 10000 1
                    , diagLen = 6
                    , diagLevel = ErrorLevel
                    , diagMsg = "msg"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: msg"
                , "    --> test.c:10000:1"
                , "     |"
                , "10000| target line"
                , "     | ^^^^^^"
                ]

        it "renders a simple error without source context" $ do
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 1
                    , diagLen = 5
                    , diagLevel = ErrorLevel
                    , diagMsg = "something went wrong"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo mempty [diag]
                [ "error: something went wrong"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                ]

        it "renders an error with source context" $ do
            let source = "int x = ;"
            let cache = Map.fromList [("test.c", [source])]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 9
                    , diagLen = 1
                    , diagLevel = ErrorLevel
                    , diagMsg = "expected expression"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: expected expression"
                , "   --> test.c:1:9"
                , "    |"
                , "   1| int x = ;"
                , "    |         ^"
                ]

        it "renders an error with multiple spans" $ do
            let source = "int x = y + z;"
            let cache = Map.fromList [("test.c", [source])]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 9
                    , diagLen = 1
                    , diagLevel = ErrorLevel
                    , diagMsg = "type mismatch"
                    , diagFlag = Nothing
                    , diagSpans =
                        [ DiagnosticSpan (CimplePos "test.c" 1 9) 1 ["this is int"]
                        , DiagnosticSpan (CimplePos "test.c" 1 13) 1 ["this is float"]
                        ]
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: type mismatch"
                , "   --> test.c:1:9"
                , "    |"
                , "   1| int x = y + z;"
                , "    |         ^"
                , "    |         |"
                , "    |         this is int"
                , "    |             ^"
                , "    |             |"
                , "    |             this is float"
                ]

        it "renders footers" $ do
             let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 1
                    , diagLen = 1
                    , diagLevel = ErrorLevel
                    , diagMsg = "bad thing"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = [(NoteLevel, "did you mean something else?")]
                    }
             shouldRenderTo mempty [diag]
                [ "error: bad thing"
                , "   --> test.c:1:1"
                , "    |"
                , "    | ^"
                , "    = note: did you mean something else?"
                ]

        it "renders multiple spans on different lines" $ do
            let source = ["int x = 1;", "int y = 2;", "x + y;"]
            let cache = Map.fromList [("test.c", source)]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 3 1
                    , diagLen = 5
                    , diagLevel = ErrorLevel
                    , diagMsg = "error here"
                    , diagFlag = Nothing
                    , diagSpans =
                        [ DiagnosticSpan (CimplePos "test.c" 1 5) 1 ["decl 1"]
                        , DiagnosticSpan (CimplePos "test.c" 2 5) 1 ["decl 2"]
                        , DiagnosticSpan (CimplePos "test.c" 3 1) 5 ["usage"]
                        ]
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: error here"
                , "   --> test.c:3:1"
                , "    |"
                , "   1| int x = 1;"
                , "    |     ^"
                , "    |     |"
                , "    |     decl 1"
                , "   2| int y = 2;"
                , "    |     ^"
                , "    |     |"
                , "    |     decl 2"
                , "   3| x + y;"
                , "    | ^^^^^"
                , "    | |"
                , "    | usage"
                ]

        it "includes implicit primary span if not present in spans" $ do
            let source = "int x;"
            let cache = Map.fromList [("test.c", [source])]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 5
                    , diagLen = 1
                    , diagLevel = ErrorLevel
                    , diagMsg = "error"
                    , diagFlag = Nothing
                    , diagSpans =
                        [ DiagnosticSpan (CimplePos "test.c" 1 1) 3 ["type"]
                        ]
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: error"
                , "   --> test.c:1:5"
                , "    |"
                , "   1| int x;"
                , "    | ^^^"
                , "    | |"
                , "    | type"
                , "    |     ^"
                ]

        it "renders multiple spans in different files" $ do
            let cache = Map.fromList
                    [ ("file1.c", ["line 1 in file 1"])
                    , ("file2.h", ["line 1 in file 2"])
                    ]
            let diag = Diagnostic
                    { diagPos = CimplePos "file1.c" 1 1
                    , diagLen = 4
                    , diagLevel = ErrorLevel
                    , diagMsg = "error across files"
                    , diagFlag = Nothing
                    , diagSpans =
                        [ DiagnosticSpan (CimplePos "file1.c" 1 1) 4 ["primary"]
                        , DiagnosticSpan (CimplePos "file2.h" 1 1) 4 ["secondary"]
                        ]
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: error across files"
                , "   --> file1.c:1:1"
                , "    |"
                , "   1| line 1 in file 1"
                , "    | ^^^^"
                , "    | |"
                , "    | primary"
                , "   ::: file2.h:1:1"
                , "    |"
                , "   1| line 1 in file 2"
                , "    | ^^^^"
                , "    | |"
                , "    | secondary"
                ]

    describe "diagToText" $ do
        it "converts diagnostic to simple text format" $ do
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 10 5
                    , diagLen = 3
                    , diagLevel = WarningLevel
                    , diagMsg = "something fishy"
                    , diagFlag = Just "checks"
                    , diagSpans = []
                    , diagFooter = [(NoteLevel, "check it out")]
                    }
            let text = diagToText diag
            text `shouldBe` "test.c:10: warning: something fishy [-Wchecks]\ntest.c:10: note: check it out"

    describe "tab expansion" $ do
        it "expands tabs in source line for alignment" $ do
            -- Source: "abc\tvar"
            -- 'a' at 1, 'b' at 2, 'c' at 3.
            -- '\t' at 4. Moves cursor to next multiple of 8 + 1 => 9.
            -- "var" starts at byte 5.
            -- Visual position of "var" is 9.
            let source = "abc\tvar"
            let cache = Map.fromList [("test.c", [source])]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 5
                    , diagLen = 3
                    , diagLevel = ErrorLevel
                    , diagMsg = "found var"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            -- Expectation:
            -- Source line should have tab expanded to 5 spaces (3 chars + 5 spaces = 8).
            -- "abc     var"
            --  123456789
            --          ^
            shouldRenderTo cache [diag]
                [ "error: found var"
                , "   --> test.c:1:5"
                , "    |"
                , "   1| abc     var"
                , "    |         ^^^"
                ]

        it "handles multi-byte characters correctly" $ do
            -- Source: "/* é */ int x;"
            -- 'é' is 2 bytes (C3 A9).
            -- "/* " is 3 bytes.
            -- "é" is 2 bytes.
            -- " */ " is 4 bytes.
            -- Total bytes before 'int': 3 + 2 + 4 = 9 bytes.
            -- 'int' starts at byte 10.
            -- Visual: "/* é */ int x;"
            -- "/* " (3 cols)
            -- "é" (1 col)
            -- " */ " (4 cols)
            -- Total visual cols: 3 + 1 + 4 = 8 cols.
            -- 'int' starts at visual col 9.
            let source = "/* é */ int x;"
            let cache = Map.fromList [("test.c", [source])]
            let diag = Diagnostic
                    { diagPos = CimplePos "test.c" 1 10
                    , diagLen = 3
                    , diagLevel = ErrorLevel
                    , diagMsg = "found int"
                    , diagFlag = Nothing
                    , diagSpans = []
                    , diagFooter = []
                    }
            shouldRenderTo cache [diag]
                [ "error: found int"
                , "   --> test.c:1:10"
                , "    |"
                , "   1| /* é */ int x;"
                , "    |         ^^^"
                ]

shouldRenderTo :: HasCallStack => Map FilePath [Text] -> [Diagnostic CimplePos] -> [Text] -> Expectation
shouldRenderTo cache diags expected =
    let docs = renderPure cache diags
        opts = defaultLayoutOptions { layoutPageWidth = Unbounded }
        rendered = renderStrict $ layoutPretty opts (vsep docs)
        actualLines = map Text.stripEnd $ Text.lines rendered
    in actualLines `shouldBe` expected

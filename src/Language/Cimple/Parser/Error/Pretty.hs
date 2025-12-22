{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.Parser.Error.Pretty
    ( describeContext
    , describeExpected
    , formatParseError
    ) where

import           Data.List                   (intercalate, isPrefixOf, nub,
                                              (\\))
import           Data.Maybe                  (fromMaybe, listToMaybe, mapMaybe)
import           Data.Text                   (Text)
import           Language.Cimple.DescribeAst (describeLexeme, describeNode,
                                              getLoc)
import           Language.Cimple.Lexer       (AlexPosn (..), Context (..),
                                              Lexeme (..), ParseError (..),
                                              lexemePosn)
import           Language.Cimple.Tokens      (LexemeClass (..))

formatParseError :: ParseError -> String
formatParseError (ParseError p@(AlexPn _ line col) ctx expected l) =
    let (AlexPn _ bestLine bestCol) = if line == 0 && col == 0
            then fromMaybe p $ listToMaybe $ mapMaybe getContextPosn ctx
            else p
        getContextPosn (ContextLexeme _ (L p' _ _)) = Just p'
        getContextPosn (ContextNode _ n)           = Just (lexemePosn (getLoc n))
        getContextPosn _                            = Nothing
        ctxNames = dedupe . filter (not . null) $ map describeContext ctx
        contextStr = if null ctxNames then "" else " while parsing " ++ intercalate " > " (reverse ctxNames)
        lexemeStr = " near " ++ describeLexeme l
        expectedStr = "; expected " ++ describeExpected expected
        hint = getHint ctx expected l
    in ":" ++ show bestLine ++ ":" ++ show bestCol ++ ": Parse error" ++ contextStr ++ lexemeStr ++ expectedStr ++ hint

dedupe :: Eq a => [a] -> [a]
dedupe []     = []
dedupe (x:xs) = x : dedupe (dropWhile (== x) xs)

getHint :: [Context] -> [String] -> Lexeme Text -> String
getHint ctx expected l
    | isTerminator l =
        let closers = nub $ filter isCloser expected ++ mapMaybe (closerForCtx . getContextName) ctx
        in if null closers
           then cimpleHint
           else "\nHint: Reached a terminator before finding the expected closing " ++ intercalate " or " closers ++ "."
    | isKeyword l && any isIdentifierToken expected =
        "\nHint: " ++ describeLexeme l ++ " is a reserved keyword and cannot be used as an identifier."
    | isIdentifier l && any isIdentifierToken expected =
        let expectedDesc = describeExpected (filter isIdentifierToken expected)
            foundDesc = describeLexeme l
        in "\nHint: Found " ++ foundDesc ++ ", but here we expected a " ++ expectedDesc ++ "."
    | otherwise = cimpleHint
  where
    cimpleHint = if any ((== "Endif") . getContextName) ctx && any ("'/*'" `isPrefixOf`) expected
                 then "\nHint: In Cimple, every #endif must be followed by a comment indicating what it closes (e.g. '#endif /* FLAG */')."
                 else ""

getContextName :: Context -> String
getContextName (Context name)         = name
getContextName (ContextLexeme name _) = name
getContextName (ContextNode name _)   = name

isIdentifier :: Lexeme Text -> Bool
isIdentifier (L _ c _) = c `elem` [IdVar, IdSueType, IdConst, IdFuncType, IdStdType]

isIdentifierToken :: String -> Bool
isIdentifierToken s = s `elem` ["ID_VAR", "ID_SUE_TYPE", "ID_CONST", "ID_FUNC_TYPE", "ID_STD_TYPE"]

isKeyword :: Lexeme Text -> Bool
isKeyword (L _ c _) = c >= KwBitwise && c <= KwWhile

closerForCtx :: String -> Maybe String
closerForCtx name = case name of
    "FunctionCall"                    -> Just "')'"
    "ArgList"                         -> Just "')'"
    "CompoundStmt"                    -> Just "'}'"
    "AggregateDecl"                   -> Just "'}'"
    "MemberDecls"                     -> Just "'}'"
    "EnumDecl"                        -> Just "'}'"
    "PostfixExpr"                     -> Just "']'" -- Often array access
    "PrimaryExpr"                     -> Just "')'" -- Parenthesized expr
    "ParenthesizedExpr"               -> Just "')'"
    "FunctionPrototype(ID_VAR)"       -> Just "')'"
    "FunctionPrototype(ID_FUNC_TYPE)" -> Just "')'"
    _                                 -> Nothing

isTerminator :: Lexeme Text -> Bool
isTerminator (L _ c _) = c `elem` [PctSemicolon, Eof, PpNewline]

isCloser :: String -> Bool
isCloser s = s `elem` ["')'", "'}'", "']'", "'*/'", "'@}'"]

describeContext :: Context -> String
describeContext (Context name) = describeContextName name
describeContext (ContextLexeme name l) =
    let name' = describeContextName name
        lex' = describeLexeme l
    in if null name' then lex'
       else if isIdentifier l then name' ++ " " ++ lex'
       else name'
describeContext (ContextNode name n) =
    let name' = describeContextName name
        node' = describeNode n
    in if null name' then node' else name' ++ " " ++ node'

describeContextName :: String -> String
describeContextName name = case name of
    "TranslationUnit"                         -> ""
    "ToplevelDecl"                            -> "top-level declaration"
    "Endif"                                   -> "#endif"
    "FunctionDecl"                            -> "function"
    "FunctionDefn"                            -> "function"
    "FunctionDeclarator"                      -> "function"
    "CompoundStmt"                            -> "block"
    "Stmt"                                    -> "statement"
    "VarDeclStmt"                             -> "variable declaration"
    "VarDecl"                                 -> "variable declaration"
    "MemberDecls"                             -> "struct/union members"
    "MemberDecl"                              -> "struct/union member"
    "IfStmt(CompoundStmt)"                    -> "if statement"
    "IfStmt(ReturnStmt)"                      -> "if statement"
    "ForStmt"                                 -> "for loop"
    "WhileStmt"                               -> "while loop"
    "DoWhileStmt"                             -> "do-while loop"
    "SwitchStmt"                              -> "switch statement"
    "AggregateDecl"                           -> "struct/union definition"
    "AggregateType"                           -> "struct/union definition"
    "EnumDecl"                                -> "enum definition"
    "ConstExpr"                               -> "constant expression"
    "Expr"                                    -> "expression"
    "PrimaryExpr"                             -> "expression"
    "FunctionCall"                            -> "function call"
    "MacroBody"                               -> "macro body"
    "FunctionPrototype(ID_VAR)"               -> "function"
    "FunctionPrototype(ID_FUNC_TYPE)"         -> "function"
    "PreprocIf(Stmts)"                        -> "#if block"
    "PreprocIfdef(Stmts)"                     -> "#ifdef block"
    "PreprocIf(ToplevelDecls)"                -> "#if block"
    "PreprocIfdef(ToplevelDecls)"             -> "#ifdef block"
    "Enumerator"                              -> "enumerator"
    "EnumeratorName"                          -> "enumerator"
    "InitialiserList"                         -> "initializer list"
    "QualType(LocalLeafType)"                 -> "type"
    "QualType(GlobalLeafType)"                -> "type"
    "LocalLeafType"                           -> "type"
    "GlobalLeafType"                          -> "type"
    _ | "FunctionPrototype" `isPrefixOf` name -> "function"
    _ | "PreprocIfdef" `isPrefixOf` name      -> "#ifdef block"
    _ | "PreprocIf" `isPrefixOf` name         -> "#if block"
    _ | "IfStmt" `isPrefixOf` name            -> "if statement"
    _ | "push_" `isPrefixOf` name             -> ""
    _                                         -> name

describeExpected :: [String] -> String
describeExpected [] = "end of file"
describeExpected [option] = describeTokenName option
describeExpected options
    | allComment options = "a comment"
    | wants ["break", "const", "continue", "ID_CONST", "VLA"] = "statement or declaration"
    | wants ["ID_FUNC_TYPE", "non_null", "static", "'#include'"] = "top-level declaration or definition"
    | options == ["ID_FUNC_TYPE", "ID_STD_TYPE", "ID_SUE_TYPE", "struct", "union", "void"] = "top-level type specifier"
    | options == ["ID_STD_TYPE", "ID_SUE_TYPE", "struct", "union", "void"] = "type specifier"
    | options == ["ID_STD_TYPE", "ID_SUE_TYPE", "bitwise", "const", "force", "struct", "union", "void"] = "type specifier"
    | options == ["ID_CONST", "ID_VAR", "LIT_CHAR", "LIT_FLOAT", "LIT_FALSE", "LIT_INTEGER", "'{'"] = "constant or literal"
    | ["ID_FUNC_TYPE", "ID_STD_TYPE", "ID_SUE_TYPE", "ID_VAR"] `isPrefixOf` options = "type specifier or variable name"
    | ["ID_FUNC_TYPE", "ID_STD_TYPE", "ID_SUE_TYPE", "bitwise", "const"] `isPrefixOf` options = "type specifier"
    | ["ID_CONST", "sizeof", "LIT_CHAR", "LIT_FALSE", "LIT_INTEGER"] `isPrefixOf` options = "constant expression"
    | ["ID_CONST", "ID_SUE_TYPE", "'/*'" ] `isPrefixOf` options = "enumerator, type name, or comment"
    | wants ["'defined'"] = "preprocessor constant expression"
    | wants ["'&'", "'&&'", "'*'", "'=='", "';'"] = "operator or end of statement"
    | wants ["'&'", "'&&'", "'*'", "'^'", "'!='"] = "operator"
    | wants ["ID_CONST", "ID_VAR", "LIT_CHAR", "'*'", "'('"] = "expression"
    | ["ID_CONST", "ID_STD_TYPE", "ID_SUE_TYPE", "ID_VAR", "const", "sizeof"] `isPrefixOf` options = "expression or type specifier"
    | ["ID_CONST", "ID_STD_TYPE", "ID_SUE_TYPE", "const", "sizeof"] `isPrefixOf` options = "constant expression or type specifier"
    | ["'&='", "'->'", "'*='"] `isPrefixOf` options = "assignment or member/array access"
    | wants ["'='", "'*='", "'/='", "'+='", "'-='"] = "assignment operator"
    | wants ["CMT_WORD"] = "comment contents"

    | length options == 2 = commaOr options
    | otherwise           = "one of " <> commaOr options
  where
    allComment opts = not (null opts) && all (`elem` ["'/*'", "'/**'", "'/***'", "'/** @{'", "'/** @} */'", "IGN_START"]) opts
    wants xs = null (xs \\ options)

describeTokenName :: String -> String
describeTokenName name = case name of
    "ID_VAR"       -> "variable name"
    "ID_SUE_TYPE"  -> "type name"
    "ID_CONST"     -> "constant name"
    "ID_FUNC_TYPE" -> "function type name"
    "ID_STD_TYPE"  -> "standard type name"
    _              -> name

commaOr :: [String] -> String
commaOr = go . reverse
  where
    go []     = ""
    go (x:xs) = intercalate ", " (reverse xs) <> " or " <> x

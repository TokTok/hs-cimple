{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Cimple.Pretty (ppTranslationUnit, showNode) where

import           Data.Fix                     (foldFix)
import qualified Data.List                    as List
import qualified Data.List.Split              as List
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (AssignOp (..), BinaryOp (..),
                                               CommentStyle (..), Lexeme (..),
                                               LexemeClass (..), Node,
                                               NodeF (..), Scope (..),
                                               UnaryOp (..), lexemeText)
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen hiding (semi)

ppText :: Text -> Doc
ppText = text . Text.unpack

ppLexeme :: Lexeme Text -> Doc
ppLexeme = ppText . lexemeText

ppSep :: Doc -> [Doc] -> [Doc]
ppSep s = List.intersperse s

commaSep :: [Doc] -> [Doc]
commaSep = punctuate (char ',')

lineSep :: [Doc] -> [Doc]
lineSep = ppSep linebreak

ppScope :: Scope -> Doc
ppScope = \case
    Global -> empty
    Static -> text "static "

ppAssignOp :: AssignOp -> Doc
ppAssignOp = \case
    AopEq     -> char '='
    AopMul    -> text "*="
    AopDiv    -> text "/="
    AopPlus   -> text "+="
    AopMinus  -> text "-="
    AopBitAnd -> text "&="
    AopBitOr  -> text "|="
    AopBitXor -> text "^="
    AopMod    -> text "%="
    AopLsh    -> text ">>="
    AopRsh    -> text "<<="

ppBinaryOp :: BinaryOp -> Doc
ppBinaryOp = \case
    BopNe     -> text "!="
    BopEq     -> text "=="
    BopOr     -> text "||"
    BopBitXor -> char '^'
    BopBitOr  -> char '|'
    BopAnd    -> text "&&"
    BopBitAnd -> char '&'
    BopDiv    -> char '/'
    BopMul    -> char '*'
    BopMod    -> char '%'
    BopPlus   -> char '+'
    BopMinus  -> char '-'
    BopLt     -> char '<'
    BopLe     -> text "<="
    BopLsh    -> text "<<"
    BopGt     -> char '>'
    BopGe     -> text ">="
    BopRsh    -> text ">>"

ppUnaryOp :: UnaryOp -> Doc
ppUnaryOp = \case
    UopNot     -> char '!'
    UopNeg     -> char '~'
    UopMinus   -> char '-'
    UopAddress -> char '&'
    UopDeref   -> char '*'
    UopIncr    -> text "++"
    UopDecr    -> text "--"

ppCommentStyle :: CommentStyle -> Doc
ppCommentStyle = \case
    Block   -> text "/***"
    Doxygen -> text "/**"
    Regular -> text "/*"

ppCommentBody :: [Lexeme Text] -> Doc
ppCommentBody = vsep . map (hsep . map ppWord) . groupLines
  where
    groupLines = List.splitWhen $ \case
        L _ PpNewline _ -> True
        _               -> False

    ppWord (L _ CmtIndent _) = char '*'
    ppWord (L _ _         t) = ppText t

ppComment :: CommentStyle -> [Lexeme Text] -> Lexeme Text -> Doc
ppComment style cs (L l c _) =
    nest 1 (ppCommentStyle style <+> ppCommentBody (cs ++ [L l c "*/"]))

ppInitialiserList :: [Doc] -> Doc
ppInitialiserList l = char '{' <+> hsep (commaSep l) <+> char '}'

ppFunctionParamList :: [Doc] -> Doc
ppFunctionParamList xs = char '(' <> hsep (commaSep xs) <> char ')'

ppFunctionPrototype
    :: Doc
    -> Lexeme Text
    -> [Doc]
    -> Doc
ppFunctionPrototype ty name params =
    ty <+> ppLexeme name <> ppFunctionParamList params

ppFunctionCall :: Doc -> [Doc] -> Doc
ppFunctionCall callee args =
    callee <> char '(' <> hsep (commaSep args) <> char ')'

ppMacroParamList :: [Doc] -> Doc
ppMacroParamList xs = char '(' <> hsep (commaSep xs) <> char ')'

ppIfStmt
    :: Doc
    -> Doc
    -> Maybe Doc
    -> Doc
ppIfStmt cond t Nothing =
    text "if (" <> cond <> text ")" <+> t
ppIfStmt cond t (Just e) =
    text "if (" <> cond <> text ")" <+> t <+> text "else" <+> e

ppForStmt
    :: Doc
    -> Doc
    -> Doc
    -> Doc
    -> Doc
ppForStmt i c n body =
    text "for ("
    <> i
    <+> c <> char ';'
    <+> n
    <> char ')' <+>
    body

ppWhileStmt
    :: Doc
    -> Doc
    -> Doc
ppWhileStmt c body =
    text "while ("
    <> c
    <> char ')' <+>
    body

ppDoWhileStmt
    :: Doc
    -> Doc
    -> Doc
ppDoWhileStmt body c =
    text "do" <+> body
    <+> text "while (" <> c <> text ");"

ppSwitchStmt
    :: Doc
    -> [Doc]
    -> Doc
ppSwitchStmt c body =
    nest 2 (
        text "switch ("
        <> c
        <> text ") {" <$>
        vcat body
    ) <$> char '}'

ppVLA :: Doc -> Lexeme Text -> Doc -> Doc
ppVLA ty n sz =
    text "VLA("
        <> ty
        <> text ", "
        <> ppLexeme n
        <> text ", "
        <> sz
        <> text ");"

ppCompoundStmt :: [Doc] -> Doc
ppCompoundStmt body =
    nest 2 (
        char '{' <$>
        ppToplevel body
    ) <$> char '}'

ppTernaryExpr
    :: Doc
    -> Doc
    -> Doc
    -> Doc
ppTernaryExpr c t e =
    c <+> char '?' <+> t <+> char ':' <+> e

ppLicenseDecl :: Lexeme Text -> [Doc] -> Doc
ppLicenseDecl l cs =
    ppCommentStyle Regular <+> text "SPDX-License-Identifier: " <> ppLexeme l <$>
    hcat (lineSep cs) <$>
    text " */"

ppNode :: Node (Lexeme Text) -> Doc
ppNode = foldFix go
  where
  go :: NodeF (Lexeme Text) Doc -> Doc
  go = \case
    StaticAssert cond msg ->
        text "static_assert(" <> cond <> char ',' <+> ppLexeme msg <> text ");"

    LicenseDecl l cs -> ppLicenseDecl l cs
    CopyrightDecl from (Just to) owner ->
        text " * Copyright © " <> ppLexeme from <> char '-' <> ppLexeme to <+>
        ppCommentBody owner
    CopyrightDecl from Nothing owner ->
        text " * Copyright © " <> ppLexeme from <+>
        ppCommentBody owner

    Comment style _ cs e ->
        ppComment style cs e
    CommentSectionEnd cs ->
        ppLexeme cs
    Commented c d ->
        c <$> d

    VarExpr var       -> ppLexeme var
    LiteralExpr _ l   -> ppLexeme l
    SizeofExpr arg    -> text "sizeof(" <> arg <> char ')'
    SizeofType arg    -> text "sizeof(" <> arg <> char ')'
    BinaryExpr  l o r -> l <+> ppBinaryOp o <+> r
    AssignExpr  l o r -> l <+> ppAssignOp o <+> r
    TernaryExpr c t e -> ppTernaryExpr c t e
    UnaryExpr o e     -> ppUnaryOp o <> e
    ParenExpr e       -> char '(' <> e <> char ')'
    FunctionCall c  a -> ppFunctionCall c a
    ArrayAccess  e  i -> e <> char '[' <> i <> char ']'
    CastExpr     ty e -> char '(' <> ty <> char ')' <> e
    CompoundExpr ty e -> char '(' <> ty <> char ')' <+> char '{' <> e <> char '}'
    PreprocDefined  n -> text "defined(" <> ppLexeme n <> char ')'
    InitialiserList l -> ppInitialiserList l
    PointerAccess e m -> e <> text "->" <> ppLexeme m
    MemberAccess  e m -> e <> text "." <> ppLexeme m
    CommentExpr   c e -> c <+> e
    Ellipsis          -> text "..."

    VarDecl ty name arrs      -> ty <+> ppLexeme name <> hcat (ppSep empty arrs)
    DeclSpecArray Nothing     -> text "[]"
    DeclSpecArray (Just dim)  -> char '[' <> dim <> char ']'

    TyPointer     ty -> ty <> char '*'
    TyConst       ty -> ty <+> text "const"
    TyUserDefined l  -> ppLexeme l
    TyStd         l  -> ppLexeme l
    TyFunc        l  -> ppLexeme l
    TyStruct      l  -> text "struct" <+> ppLexeme l

    ExternC decls ->
        text "#ifdef __cplusplus" <$>
        text "extern \"C\" {" <$>
        text "#endif" <$>
        line <>
        ppToplevel decls <$>
        line <>
        text "#ifdef __cplusplus" <$>
        text "}" <$>
        text "#endif"

    MacroParam l -> ppLexeme l

    MacroBodyFunCall e -> e
    MacroBodyStmt body ->
        if False
           then text "do" <+> body <+> text "while (0)"
           else text "do { nothing(); } while (0)  // macros aren't supported well yet"

    PreprocScopedDefine def stmts undef ->
        def <$> ppToplevel stmts <$> undef

    PreprocInclude hdr ->
        text "#include" <+> ppLexeme hdr
    PreprocDefine name ->
        text "#define" <+> ppLexeme name
    PreprocDefineConst name value ->
        text "#define" <+> ppLexeme name <+> value
    PreprocDefineMacro name params body ->
        text "#define" <+> ppLexeme name <> ppMacroParamList params <+> body
    PreprocUndef name ->
        text "#undef" <+> ppLexeme name

    PreprocIf cond decls elseBranch ->
        text "#if" <+> cond <$>
        ppToplevel decls <>
        elseBranch <$>
        text "#endif"
    PreprocIfdef name decls elseBranch ->
        text "#ifdef" <+> ppLexeme name <$>
        ppToplevel decls <>
        elseBranch <$>
        text "#endif"
    PreprocIfndef name decls elseBranch ->
        text "#ifndef" <+> ppLexeme name <$>
        ppToplevel decls <>
        elseBranch <$>
        text "#endif"
    PreprocElse [] -> empty
    PreprocElse decls ->
        linebreak <>
        text "#else" <$>
        ppToplevel decls
    PreprocElif cond decls elseBranch ->
        hardline <>
        text "#elif" <+> cond <$>
        ppToplevel decls <>
        elseBranch

    FunctionPrototype ty name params ->
        ppFunctionPrototype ty name params
    FunctionDecl scope proto ->
        ppScope scope <> proto <> char ';'
    FunctionDefn scope proto body ->
        ppScope scope <> proto <+> body

    MemberDecl decl Nothing ->
        decl <> char ';'
    MemberDecl decl (Just size) ->
        decl <+> char ':' <+> ppLexeme size <> char ';'

    AggregateDecl struct -> struct <> char ';'
    Struct name members ->
        nest 2 (
            text "struct" <+> ppLexeme name <+> char '{' <$>
            ppToplevel members
        ) <$> char '}'
    Union name members ->
        nest 2 (
            text "union" <+> ppLexeme name <+> char '{' <$>
            ppToplevel members
        ) <$> char '}'
    Typedef ty tyname ->
        text "typedef" <+> ty <+> ppLexeme tyname <> char ';'
    TypedefFunction proto ->
        text "typedef" <+> proto <> char ';'

    ConstDecl ty name ->
        text "extern const" <+> ty <+> ppLexeme name <> char ';'
    ConstDefn scope ty name value ->
        ppScope scope <> text "const" <+>
        ty <+> ppLexeme name <+> char '=' <+> value <> char ';'

    Enumerator name  Nothing -> ppLexeme name <> char ','
    Enumerator name (Just value) ->
        ppLexeme name <+> char '=' <+> value <> char ','

    EnumConsts Nothing enums ->
        nest 2 (
            text "enum" <+> char '{' <$>
            hcat (lineSep enums)
        ) <$> text "};"
    EnumConsts (Just name) enums ->
        nest 2 (
            text "enum" <+> ppLexeme name <+> char '{' <$>
            hcat (lineSep enums)
        ) <$> text "};"
    EnumDecl name enums ty ->
        nest 2 (
            text "typedef enum" <+> ppLexeme name <+> char '{' <$>
            hcat (lineSep enums)
        ) <$> text "} " <> ppLexeme ty <> char ';'

    -- Statements
    VarDeclStmt decl Nothing      -> decl <> char ';'
    VarDeclStmt decl (Just initr) -> decl <+> char '=' <+> initr <> char ';'
    Return Nothing                -> text "return;"
    Return (Just e)               -> text "return" <+> e <> char ';'
    Continue                      -> text "continue;"
    Break                         -> text "break;"
    IfStmt cond t e               -> ppIfStmt cond t e
    ForStmt i c n body            -> ppForStmt i c n body
    Default s                     -> text "default:" <+> s
    Label l s                     -> ppLexeme l <> char ':' <$> s
    ExprStmt e                    -> e <> char ';'
    Goto l                        -> text "goto " <> ppLexeme l <> char ';'
    Case e s                      -> text "case " <> e <> char ':' <+> s
    WhileStmt c body              -> ppWhileStmt c body
    DoWhileStmt body c            -> ppDoWhileStmt body c
    SwitchStmt c body             -> ppSwitchStmt c body
    CompoundStmt body             -> ppCompoundStmt body
    VLA ty n sz                   -> ppVLA ty n sz


ppToplevel :: [Doc] -> Doc
ppToplevel = vcat . punctuate line

ppTranslationUnit :: [Node (Lexeme Text)] -> Doc
ppTranslationUnit decls = (ppToplevel . map ppNode $ decls) <> linebreak

showNode  :: Node (Lexeme Text) -> Text
showNode = Text.pack . show . ppNode

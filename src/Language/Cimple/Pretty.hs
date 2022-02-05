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

-- | Whether a node needs a semicolon at the end when it's a statement or
-- declaration.
data NeedsSemi
    = SemiNo
    | SemiYes

-- | Annotated Doc which is passed upwards through the fold. 'fst' is the
-- accumulated pretty-printed code. 'snd' states whether the current statement
-- should end in a semicolon ';'. E.g. function definitions don't, while
-- function declarations do.
type ADoc = (Doc, NeedsSemi)
bare, semi :: Doc -> ADoc
bare = (, SemiNo)
semi = (, SemiYes)

-- | Copy the 'NeedsSemi' from another 'ADoc' to a newly created doc.
cp :: ADoc -> Doc -> ADoc
cp (_, s) d = (d, s)

ppText :: Text -> Doc
ppText = text . Text.unpack

ppLexeme :: Lexeme Text -> Doc
ppLexeme = ppText . lexemeText

ppSep :: Doc -> [ADoc] -> [Doc]
ppSep s = List.intersperse s . map fst

commaSep :: [ADoc] -> [Doc]
commaSep = punctuate (char ',') . map fst

lineSep :: [ADoc] -> [Doc]
lineSep = ppSep linebreak

semiSep :: [ADoc] -> [Doc]
semiSep = map $ addEnd $ char ';'
  where
    addEnd s (d, SemiYes) = d <> s
    addEnd _ (d, SemiNo)  = d

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

ppInitialiserList :: [ADoc] -> Doc
ppInitialiserList l = char '{' <+> hsep (commaSep l) <+> char '}'

ppFunctionParamList :: [ADoc] -> Doc
ppFunctionParamList xs = char '(' <> hsep (commaSep xs) <> char ')'

ppFunctionPrototype
    :: ADoc
    -> Lexeme Text
    -> [ADoc]
    -> Doc
ppFunctionPrototype ty name params =
    fst ty <+> ppLexeme name <> ppFunctionParamList params

ppFunctionCall :: ADoc -> [ADoc] -> Doc
ppFunctionCall callee args =
    fst callee <> char '(' <> hsep (commaSep args) <> char ')'

ppMacroParamList :: [ADoc] -> Doc
ppMacroParamList xs = char '(' <> hsep (commaSep xs) <> char ')'

ppIfStmt
    :: ADoc
    -> ADoc
    -> Maybe ADoc
    -> Doc
ppIfStmt cond t Nothing =
    text "if (" <> fst cond <> text ")" <+> fst t
ppIfStmt cond t (Just e) =
    text "if (" <> fst cond <> text ")" <+> fst t <+> text "else" <+> fst e

ppForStmt
    :: ADoc
    -> ADoc
    -> ADoc
    -> ADoc
    -> Doc
ppForStmt i c n body =
    text "for ("
    <> fst i <> char ';'
    <+> fst c <> char ';'
    <+> fst n
    <> char ')' <+>
    fst body

ppWhileStmt
    :: ADoc
    -> ADoc
    -> Doc
ppWhileStmt c body =
    text "while ("
    <> fst c
    <> char ')' <+>
    fst body

ppDoWhileStmt
    :: ADoc
    -> ADoc
    -> Doc
ppDoWhileStmt body c =
    text "do ("
    <> text ") {" <$>
    fst body
    <+> text "while (" <> fst c <> char ')'

ppSwitchStmt
    :: ADoc
    -> [ADoc]
    -> Doc
ppSwitchStmt c body =
    nest 2 (
        text "switch ("
        <> fst c
        <> text ") {" <$>
        vcat (semiSep body)
    ) <$> char '}'

ppVLA :: ADoc -> Lexeme Text -> ADoc -> Doc
ppVLA ty n sz =
    text "VLA("
        <> fst ty
        <> text ", "
        <> ppLexeme n
        <> text ", "
        <> fst sz
        <> char ')'

ppCompoundStmt :: [ADoc] -> Doc
ppCompoundStmt body =
    nest 2 (
        char '{' <$>
        ppToplevel body
    ) <$> char '}'

ppTernaryExpr
    :: ADoc
    -> ADoc
    -> ADoc
    -> Doc
ppTernaryExpr c t e =
    fst c <+> char '?' <+> fst t <+> char ':' <+> fst e

ppLicenseDecl :: Lexeme Text -> [ADoc] -> Doc
ppLicenseDecl l cs =
    ppCommentStyle Regular <+> text "SPDX-License-Identifier: " <> ppLexeme l <$>
    hcat (lineSep cs) <$>
    text " */"

ppNode :: Node (Lexeme Text) -> ADoc
ppNode = foldFix go
  where
  go :: NodeF (Lexeme Text) ADoc -> ADoc
  go = \case
    StaticAssert cond msg -> semi $
        text "static_assert(" <+> fst cond <> char ',' <+> ppLexeme msg <> char ')'

    LicenseDecl l cs -> bare $ ppLicenseDecl l cs
    CopyrightDecl from (Just to) owner -> bare $
        text " * Copyright © " <> ppLexeme from <> char '-' <> ppLexeme to <+>
        ppCommentBody owner
    CopyrightDecl from Nothing owner -> bare $
        text " * Copyright © " <> ppLexeme from <+>
        ppCommentBody owner

    Comment style _ cs e -> bare $
        ppComment style cs e
    CommentSectionEnd cs -> bare $
        ppLexeme cs
    Commented (c, _) (d, s) -> (, s) $
        c <$> d

    VarExpr var       -> semi $ ppLexeme var
    LiteralExpr _ l   -> semi $ ppLexeme l
    SizeofExpr arg    -> semi $ text "sizeof(" <> fst arg <> char ')'
    SizeofType arg    -> semi $ text "sizeof(" <> fst arg <> char ')'
    BinaryExpr  l o r -> semi $ fst l <+> ppBinaryOp o <+> fst r
    AssignExpr  l o r -> semi $ fst l <+> ppAssignOp o <+> fst r
    TernaryExpr c t e -> semi $ ppTernaryExpr c t e
    UnaryExpr o e     -> semi $ ppUnaryOp o <> fst e
    ParenExpr e       -> semi $ char '(' <> fst e <> char ')'
    FunctionCall c  a -> semi $ ppFunctionCall c a
    ArrayAccess  e  i -> semi $ fst e <> char '[' <> fst i <> char ']'
    CastExpr     ty e -> semi $ char '(' <> fst ty <> char ')' <> fst e
    CompoundExpr ty e -> semi $ char '(' <> fst ty <> char ')' <+> char '{' <> fst e <> char '}'
    PreprocDefined  n -> bare $ text "defined(" <> ppLexeme n <> char ')'
    InitialiserList l -> semi $ ppInitialiserList l
    PointerAccess e m -> semi $ fst e <> text "->" <> ppLexeme m
    MemberAccess  e m -> semi $ fst e <> text "." <> ppLexeme m
    CommentExpr   c e -> semi $ fst c <+> fst e
    Ellipsis          -> semi $ text "..."

    VarDecl ty name arrs      -> bare $ fst ty <+> ppLexeme name <> hcat (ppSep empty arrs)
    DeclSpecArray Nothing     -> bare $ text "[]"
    DeclSpecArray (Just dim)  -> bare $ char '[' <> fst dim <> char ']'

    TyPointer     ty -> bare $ fst ty <> char '*'
    TyConst       ty -> bare $ fst ty <+> text "const"
    TyUserDefined l  -> bare $ ppLexeme l
    TyStd         l  -> bare $ ppLexeme l
    TyFunc        l  -> bare $ ppLexeme l
    TyStruct      l  -> bare $ text "struct" <+> ppLexeme l

    ExternC decls -> bare $
        text "#ifndef __cplusplus" <$>
        text "extern \"C\" {" <$>
        text "#endif" <$>
        line <>
        ppToplevel decls <$>
        line <>
        text "#ifndef __cplusplus" <$>
        text "}" <$>
        text "#endif"

    MacroParam l -> bare $ ppLexeme l

    MacroBodyFunCall e -> e
    MacroBodyStmt body -> bare $
        text "do" <+> fst body <+> text "while (0)"

    PreprocScopedDefine def stmts undef -> bare $
        fst def <$> ppToplevel stmts <$> fst undef

    PreprocInclude hdr -> bare $
        text "#include" <+> ppLexeme hdr
    PreprocDefine name -> bare $
        text "#define" <+> ppLexeme name
    PreprocDefineConst name value -> bare $
        text "#define" <+> ppLexeme name <+> fst value
    PreprocDefineMacro name params body -> bare $
        text "#define" <+> ppLexeme name <> ppMacroParamList params <+> fst body
    PreprocUndef name -> bare $
        text "#undef" <+> ppLexeme name

    PreprocIf cond decls elseBranch -> bare $
        text "#if" <+> fst cond <$>
        ppToplevel decls <>
        fst elseBranch <$>
        text "#endif"
    PreprocIfdef name decls elseBranch -> bare $
        text "#ifdef" <+> ppLexeme name <$>
        ppToplevel decls <>
        fst elseBranch <$>
        text "#endif"
    PreprocIfndef name decls elseBranch -> bare $
        text "#ifndef" <+> ppLexeme name <$>
        ppToplevel decls <>
        fst elseBranch <$>
        text "#endif"
    PreprocElse [] -> bare empty
    PreprocElse decls -> bare $
        linebreak <>
        text "#else" <$>
        ppToplevel decls
    PreprocElif cond decls elseBranch -> bare $
        text "#elif" <+> fst cond <$>
        ppToplevel decls <>
        fst elseBranch <$>
        text "#endif"

    FunctionPrototype ty name params -> bare $
        ppFunctionPrototype ty name params
    FunctionDecl scope proto -> semi $
        ppScope scope <> fst proto
    FunctionDefn scope proto body -> bare $
        ppScope scope <> fst proto <+> fst body

    MemberDecl decl Nothing -> semi $
        fst decl
    MemberDecl decl (Just size) -> semi $
        fst decl <+> char ':' <+> ppLexeme size

    Struct name members -> semi $
        nest 2 (
            text "struct" <+> ppLexeme name <+> char '{' <$>
            ppToplevel members
        ) <$> char '}'
    Union name members -> semi $
        nest 2 (
            text "union" <+> ppLexeme name <+> char '{' <$>
            ppToplevel members
        ) <$> char '}'
    Typedef ty tyname -> semi $
        text "typedef" <+> fst ty <+> ppLexeme tyname
    TypedefFunction proto -> semi $
        text "typedef" <+> fst proto

    ConstDecl ty name -> semi $
        text "extern const" <+> fst ty <+> ppLexeme name
    ConstDefn scope ty name value -> semi $
        ppScope scope <> text "const" <+>
        fst ty <+> ppLexeme name <+> char '=' <+> fst value

    Enumerator name  Nothing -> bare $ ppLexeme name <> char ','
    Enumerator name (Just value) -> bare $
        ppLexeme name <+> char '=' <+> fst value <> char ','

    EnumConsts Nothing enums -> semi $
        nest 2 (
            text "enum" <+> char '{' <$>
            hcat (lineSep enums)
        ) <$> char '}'
    EnumConsts (Just name) enums -> semi $
        nest 2 (
            text "enum" <+> ppLexeme name <+> char '{' <$>
            hcat (lineSep enums)
        ) <$> char '}'
    EnumDecl name enums ty -> semi $
        nest 2 (
            text "typedef enum" <+> ppLexeme name <+> char '{' <$>
            hcat (lineSep enums)
        ) <$> text "} " <> ppLexeme ty

    -- Statements
    VarDeclStmt decl Nothing      -> semi $ fst decl
    VarDeclStmt decl (Just initr) -> semi $ fst decl <+> char '=' <+> fst initr
    Return Nothing                -> semi $ text "return"
    Return (Just e)               -> semi $ text "return" <+> fst e
    Continue                      -> semi $ text "continue"
    Break                         -> semi $ text "break"
    IfStmt cond t e               -> bare $ ppIfStmt cond t e
    ForStmt i c n body            -> bare $ ppForStmt i c n body
    Default s                     -> cp s $ text "default:" <+> fst s
    Label l s                     -> cp s $ ppLexeme l <> char ':' <$> fst s
    Goto l                        -> semi $ text "goto " <> ppLexeme l
    Case e s                      -> cp s $ text "case " <> fst e <> char ':' <+> fst s
    WhileStmt c body              -> bare $ ppWhileStmt c body
    DoWhileStmt body c            -> semi $ ppDoWhileStmt body c
    SwitchStmt c body             -> bare $ ppSwitchStmt c body
    CompoundStmt body             -> bare $ ppCompoundStmt body
    VLA ty n sz                   -> semi $ ppVLA ty n sz


ppToplevel :: [ADoc] -> Doc
ppToplevel = vcat . punctuate line . semiSep

ppTranslationUnit :: [Node (Lexeme Text)] -> Doc
ppTranslationUnit decls = (ppToplevel . map ppNode $ decls) <> linebreak

showNode  :: Node (Lexeme Text) -> Text
showNode = Text.pack . show . fst . ppNode

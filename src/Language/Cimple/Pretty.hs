{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Language.Cimple.Pretty (ppTranslationUnit) where

import           Data.Fix                     (foldFix)
import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (AssignOp (..), BinaryOp (..),
                                               CommentStyle (..), Lexeme (..),
                                               LexemeClass (..), Node,
                                               NodeF (..), Scope (..),
                                               UnaryOp (..), lexemeText)
import           Prelude                      hiding ((<$>))
import           Text.Groom                   (groom)
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

ppSep :: Doc -> [ADoc] -> Doc
ppSep s = foldr (<>) empty . List.intersperse s . map fst

ppCommaSep :: [ADoc] -> Doc
ppCommaSep = ppSep (text ", ")

ppLineSep :: [ADoc] -> Doc
ppLineSep = ppSep linebreak

ppSemiSep :: [ADoc] -> Doc
ppSemiSep = ppEnd (char ';')
  where
    ppEnd s = foldr (<>) empty . List.intersperse linebreak . map (addEnd s)

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
ppCommentBody = go
  where
    go (L _ LitInteger t1 : L _ PctMinus m : L _ LitInteger t2 : xs) =
        space <> ppText t1 <> ppText m <> ppText t2 <> go xs
    go (L _ PctMinus m : L _ LitInteger t : xs) =
        space <> ppText m <> ppText t <> go xs

    go (l : L _ PctPeriod t : xs) = go [l] <> ppText t <> go xs
    go (l : L _ PctComma  t : xs) = go [l] <> ppText t <> go xs
    go (x                   : xs) = ppWord x <> go xs
    go []                         = empty

    ppWord (L _ CmtSpdxLicense   t) = space <> ppText t
    ppWord (L _ CmtSpdxCopyright t) = space <> ppText t
    ppWord (L _ CmtWord          t) = space <> ppText t
    ppWord (L _ CmtCode          t) = space <> ppText t
    ppWord (L _ CmtRef           t) = space <> ppText t
    ppWord (L _ CmtIndent        _) = char '*'
    ppWord (L _ PpNewline        _) = linebreak
    ppWord (L _ LitInteger       t) = space <> ppText t
    ppWord (L _ LitString        t) = space <> ppText t
    ppWord (L _ PctEMark         t) = space <> ppText t
    ppWord (L _ PctPlus          t) = space <> ppText t
    ppWord (L _ PctEq            t) = space <> ppText t
    ppWord (L _ PctMinus         t) = space <> ppText t
    ppWord (L _ PctPeriod        t) = space <> ppText t
    ppWord (L _ PctLParen        t) = space <> ppText t
    ppWord (L _ PctRParen        t) = space <> ppText t
    ppWord (L _ PctSemicolon     t) = space <> ppText t
    ppWord (L _ PctColon         t) = space <> ppText t
    ppWord (L _ PctQMark         t) = space <> ppText t
    ppWord (L _ PctSlash         t) = space <> ppText t
    ppWord (L _ PctGreater       t) = space <> ppText t
    ppWord (L _ PctLess          t) = space <> ppText t
    ppWord (L _ PctComma         t) = space <> ppText t
    ppWord x                        = error $ "ppWord: " <> groom x

ppComment :: CommentStyle -> [Lexeme Text] -> Doc
ppComment style cs =
    nest 1 (ppCommentStyle style <> ppCommentBody cs) <+> text "*/"

ppInitialiserList :: [ADoc] -> Doc
ppInitialiserList l = char '{' <+> ppCommaSep l <+> char '}'

ppFunctionParamList :: [ADoc] -> Doc
ppFunctionParamList xs = char '(' <> ppCommaSep xs <> char ')'

ppFunctionPrototype
    :: ADoc
    -> Lexeme Text
    -> [ADoc]
    -> Doc
ppFunctionPrototype ty name params =
    fst ty <+> ppLexeme name <> ppFunctionParamList params

ppFunctionCall :: ADoc -> [ADoc] -> Doc
ppFunctionCall callee args =
    fst callee <> char '(' <> ppCommaSep args <> char ')'

ppMacroParamList :: [ADoc] -> Doc
ppMacroParamList xs = char '(' <> ppCommaSep xs <> char ')'

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
        ppSemiSep body
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
        ppSemiSep body
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
    ppLineSep cs <$>
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
        text " * Copyright © " <>
        ppLexeme from <> char '-' <> ppLexeme to <>
        ppCommentBody owner
    CopyrightDecl from Nothing owner -> bare $
        text " * Copyright © " <>
        ppLexeme from <>
        ppCommentBody owner

    Comment style _ cs _ -> bare $
        ppComment style cs
    CommentBlock cs -> bare $
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

    Declarator dspec Nothing -> dspec
    Declarator dspec (Just initr) -> bare $ fst dspec <+> char '=' <+> fst initr

    DeclSpecVar var -> bare $ ppLexeme var
    DeclSpecArray dspec Nothing     -> bare $ fst dspec <> text "[]"
    DeclSpecArray dspec (Just dim)  -> bare $ fst dspec <> char '[' <> fst dim <> char ']'

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
        ppSemiSep decls <$>
        text "#ifndef __cplusplus" <$>
        text "}" <$>
        text "#endif"

    MacroParam l -> bare $ ppLexeme l

    MacroBodyFunCall e -> e
    MacroBodyStmt body -> bare $
        text "do" <+> fst body <+> text "while (0)"

    PreprocScopedDefine def stmts undef -> bare $
        fst def <$> ppSemiSep stmts <$> fst undef

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
        ppSemiSep decls <>
        fst elseBranch <$>
        text "#endif"
    PreprocIfdef name decls elseBranch -> bare $
        text "#ifdef" <+> ppLexeme name <$>
        ppSemiSep decls <>
        fst elseBranch <$>
        text "#endif"
    PreprocIfndef name decls elseBranch -> bare $
        text "#ifndef" <+> ppLexeme name <$>
        ppSemiSep decls <>
        fst elseBranch <$>
        text "#endif"
    PreprocElse [] -> bare $ empty
    PreprocElse decls -> bare $
        linebreak <>
        text "#else" <$>
        ppSemiSep decls
    PreprocElif cond decls elseBranch -> bare $
        text "#elif" <+> fst cond <$>
        ppSemiSep decls <>
        fst elseBranch <$>
        text "#endif"

    FunctionParam ty dspec -> bare $ fst ty <+> fst dspec
    FunctionPrototype ty name params -> bare $
        ppFunctionPrototype ty name params
    FunctionDecl scope proto -> semi $
        ppScope scope <> fst proto
    FunctionDefn scope proto body -> bare $
        ppScope scope <> fst proto <+> fst body

    MemberDecl ty dspec Nothing -> semi $
        fst ty <+> fst dspec
    MemberDecl ty dspec (Just size) -> semi $
        fst ty <+> fst dspec <+> char ':' <+> ppLexeme size

    Struct name members -> semi $
        nest 2 (
            text "struct" <+> ppLexeme name <+> char '{' <$>
            ppSemiSep members
        ) <$> char '}'
    Union name members -> semi $
        nest 2 (
            text "union" <+> ppLexeme name <+> char '{' <$>
            ppSemiSep members
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
            ppLineSep enums
        ) <$> char '}'
    EnumConsts (Just name) enums -> semi $
        nest 2 (
            text "enum" <+> ppLexeme name <+> char '{' <$>
            ppLineSep enums
        ) <$> char '}'
    EnumDecl name enums ty -> semi $
        nest 2 (
            text "typedef enum" <+> ppLexeme name <+> char '{' <$>
            ppLineSep enums
        ) <$> text "} " <> ppLexeme ty

    -- Statements
    VarDecl ty declr   -> semi $ fst ty <+> fst declr
    Return Nothing     -> semi $ text "return"
    Return (Just e)    -> semi $ text "return" <+> fst e
    Continue           -> semi $ text "continue"
    Break              -> semi $ text "break"
    IfStmt cond t e    -> bare $ ppIfStmt cond t e
    ForStmt i c n body -> bare $ ppForStmt i c n body
    Default s          -> cp s $ text "default:" <+> fst s
    Label l s          -> bare $ ppLexeme l <> char ':' <$> fst s
    Goto l             -> semi $ text "goto " <> ppLexeme l
    Case e s           -> cp s $ text "case " <> fst e <> char ':' <+> fst s
    WhileStmt c body   -> bare $ ppWhileStmt c body
    DoWhileStmt body c -> semi $ ppDoWhileStmt body c
    SwitchStmt c body  -> bare $ ppSwitchStmt c body
    CompoundStmt body  -> bare $ ppCompoundStmt body
    VLA ty n sz        -> semi $ ppVLA ty n sz


ppTranslationUnit :: [Node (Lexeme Text)] -> Doc
ppTranslationUnit decls = ppSemiSep (map ppNode decls) <> linebreak

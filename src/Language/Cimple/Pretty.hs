{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.Pretty
    ( plain
    , render
    , ppTranslationUnit
    , showNode
    ) where

import           Data.Fix                     (foldFix)
import qualified Data.List.Split              as List
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (AssignOp (..), BinaryOp (..),
                                               Comment, CommentF (..),
                                               CommentStyle (..), Lexeme (..),
                                               LexemeClass (..), Node,
                                               NodeF (..), Scope (..),
                                               UnaryOp (..), lexemeLine,
                                               lexemeText)
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

indentWidth :: Int
indentWidth = 2

kwBreak         = dullred   $ text "break"
kwCase          = dullred   $ text "case"
kwConst         = dullgreen $ text "const"
kwContinue      = dullred   $ text "continue"
kwDefault       = dullred   $ text "default"
kwDo            = dullred   $ text "do"
kwElse          = dullred   $ text "else"
kwEnum          = dullgreen $ text "enum"
kwExtern        = dullgreen $ text "extern"
kwFor           = dullred   $ text "for"
kwGnuPrintf     = dullgreen $ text "GNU_PRINTF"
kwGoto          = dullred   $ text "goto"
kwIf            = dullred   $ text "if"
kwNonNull       = dullgreen $ text "non_null"
kwNullable      = dullgreen $ text "nullable"
kwReturn        = dullred   $ text "return"
kwSizeof        = dullred   $ text "sizeof"
kwStaticAssert  = dullred   $ text "static_assert"
kwStatic        = dullgreen $ text "static"
kwStruct        = dullgreen $ text "struct"
kwSwitch        = dullred   $ text "switch"
kwTypedef       = dullgreen $ text "typedef"
kwUnion         = dullgreen $ text "union"
kwWhile         = dullred   $ text "while"

kwDocAttention  = dullcyan $ text "@attention"
kwDocBrief      = dullcyan $ text "@brief"
kwDocDeprecated = dullcyan $ text "@deprecated"
kwDocParam      = dullcyan $ text "@param"
kwDocRef        = dullcyan $ text "@ref"
kwDocReturn     = dullcyan $ text "@return"
kwDocRetval     = dullcyan $ text "@retval"
kwDocP          = dullcyan $ text "@p"
kwDocSee        = dullcyan $ text "@see"

cmtPrefix :: Doc
cmtPrefix = dullyellow (char '*')

ppText :: Text -> Doc
ppText = text . Text.unpack

ppLexeme :: Lexeme Text -> Doc
ppLexeme = ppText . lexemeText

commaSep :: [Doc] -> Doc
commaSep = hsep . punctuate comma

ppScope :: Scope -> Doc
ppScope = \case
    Global -> empty
    Static -> kwStatic <> space

ppAssignOp :: AssignOp -> Doc
ppAssignOp = \case
    AopEq     -> equals
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

ppCommentStart :: CommentStyle -> Doc
ppCommentStart = dullyellow . \case
    Block   -> text "/***"
    Doxygen -> text "/**"
    Section -> text "/** @{"
    Regular -> text "/*"
    Ignore  -> text "//!TOKSTYLE-"

ppCommentBody :: [Lexeme Text] -> Doc
ppCommentBody body = vsep . prefixStars . map (hsep . map ppWord) . groupLines $ body
  where
    -- If the "*/" is on a separate line, don't add an additional "*" before
    -- it. If "*/" is on the same line, then do add a "*" prefix on the last line.
    stars =
        case reverse body of
          e:c:_ | lexemeLine e > lexemeLine c -> 2
          _                                   -> 1
    prefixStars xs = zipWith (<>) (empty : replicate (length xs - stars) cmtPrefix ++ [empty]) xs
    groupLines = List.splitWhen $ \case
        L _ PpNewline _ -> True
        _               -> False

ppWord (L _ CmtIndent  _) = empty
ppWord (L _ CmtCommand t) = dullcyan   $ ppText t
ppWord (L _ _          t) = dullyellow $ ppText t

ppComment :: CommentStyle -> [Lexeme Text] -> Lexeme Text -> Doc
ppComment Ignore cs _ =
    ppCommentStart Ignore <> hcat (map ppWord cs) <> dullyellow (text "//!TOKSTYLE+" <> line)
ppComment style cs (L l c _) =
    nest 1 $ ppCommentStart style <+> ppCommentBody (cs ++ [L l c "*/"])

ppInitialiserList :: [Doc] -> Doc
ppInitialiserList l = lbrace <+> commaSep l <+> rbrace

ppParamList :: [Doc] -> Doc
ppParamList = parens . indent 0 . commaSep

ppFunctionPrototype
    :: Doc
    -> Lexeme Text
    -> [Doc]
    -> Doc
ppFunctionPrototype ty name params =
    ty <+> ppLexeme name <> ppParamList params

ppFunctionCall :: Doc -> [Doc] -> Doc
ppFunctionCall callee args =
    callee <> ppParamList args

ppIfStmt
    :: Doc
    -> Doc
    -> Maybe Doc
    -> Doc
ppIfStmt cond t Nothing =
    kwIf <+> parens cond <+> t
ppIfStmt cond t (Just e) =
    kwIf <+> parens cond <+> t <+> kwElse <+> e

ppForStmt
    :: Doc
    -> Doc
    -> Doc
    -> Doc
    -> Doc
ppForStmt i c n body =
    kwFor <+> parens (i <+> c <> semi <+> n) <+> body

ppWhileStmt
    :: Doc
    -> Doc
    -> Doc
ppWhileStmt c body =
    kwWhile <+> parens c <+> body

ppDoWhileStmt
    :: Doc
    -> Doc
    -> Doc
ppDoWhileStmt body c =
    kwDo <+> body <+> kwWhile <+> parens c <> semi

ppSwitchStmt
    :: Doc
    -> [Doc]
    -> Doc
ppSwitchStmt c body =
    nest indentWidth (
        kwSwitch <+> parens c <+> lbrace <$>
        vcat body
    ) <$> rbrace

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
    nest indentWidth (
        lbrace <$>
        ppToplevel body
    ) <$> rbrace

ppTernaryExpr
    :: Doc
    -> Doc
    -> Doc
    -> Doc
ppTernaryExpr c t e =
    c <+> char '?' <+> t <+> colon <+> e

ppLicenseDecl :: Lexeme Text -> [Doc] -> Doc
ppLicenseDecl l cs =
    dullyellow $ ppCommentStart Regular <+> text "SPDX-License-Identifier: " <> ppLexeme l <$>
    vcat (map dullyellow cs) <$>
    dullyellow (text " */")

ppIntList :: [Lexeme Text] -> Doc
ppIntList = parens . commaSep . map (dullred . ppLexeme)

ppMacroBody :: Doc -> Doc
ppMacroBody =
    vcat
    . map dullmagenta
    . punctuate (text " \\")
    . map text
    . List.splitOn "\n"
    . renderS
    . plain

ppVerbatimComment :: Doc -> Doc
ppVerbatimComment =
    vcat
    . map dullyellow
    . zipWith (<>) (empty : repeat (text " * "))
    . map text
    . List.splitOn "\n"
    . renderS
    . plain

ppCommentInfo :: Comment (Lexeme Text) -> Doc
ppCommentInfo = foldFix go
  where
  ppBody     = vcat . zipWith (<>) (        repeat (dullyellow (text " * "  )))
  ppIndented = vcat . zipWith (<>) (empty : repeat (dullyellow (text " *   ")))
  ppRef      = underline . cyan . ppLexeme
  ppAttr     = maybe empty (blue . ppLexeme)

  go :: CommentF (Lexeme Text) Doc -> Doc
  go = dullyellow . \case
    DocComment docs ->
        text "/**" <$>
        ppBody docs <$>
        dullyellow (text " */")
    DocWord w -> ppLexeme w
    DocSentence docs ending -> fillSep docs <> ppLexeme ending
    DocNewline -> empty

    DocParam attr name docs ->
        kwDocParam <> ppAttr attr <+> underline (cyan (ppLexeme name)) <+> ppIndented docs

    DocAttention docs   -> kwDocAttention  <+> ppIndented docs
    DocBrief docs       -> kwDocBrief      <+> ppIndented docs
    DocDeprecated docs  -> kwDocDeprecated <+> ppIndented docs
    DocReturn docs      -> kwDocReturn     <+> ppIndented docs
    DocRetval expr docs -> kwDocRetval     <+> dullred (ppLexeme expr) <+> ppIndented docs
    DocSee name docs    -> kwDocSee        <+> ppRef name <+> ppIndented docs
    DocRef name         -> kwDocRef        <+> ppRef name
    DocP name           -> kwDocP          <+> ppRef name

    DocParagraph docs -> ppIndented docs
    DocLine docs -> fillSep docs
    DocList l -> ppVerbatimComment $ vcat l
    DocOLItem num docs -> ppLexeme num <> char '.' <+> nest 3 (fillSep docs)
    DocULItem docs sublist -> char '-' <+> nest 2 (vsep $ fillSep docs : sublist)

    DocLParen doc -> lparen <> doc
    DocRParen doc -> doc <> rparen
    DocColon doc -> ppLexeme doc <> char ':'
    DocBinaryOp BopMinus l r -> l <>  char '-'      <>  r
    DocBinaryOp BopDiv   l r -> l <>  char '/'      <>  r
    DocAssignOp op       l r -> l <+> ppAssignOp op <+> r
    DocBinaryOp op       l r -> l <+> ppBinaryOp op <+> r

ppNode :: Node (Lexeme Text) -> Doc
ppNode = foldFix go
  where
  go :: NodeF (Lexeme Text) Doc -> Doc
  go = \case
    StaticAssert cond msg ->
        kwStaticAssert <> parens (cond <> comma <+> dullred (ppLexeme msg)) <> semi

    LicenseDecl l cs -> ppLicenseDecl l cs
    CopyrightDecl from (Just to) owner ->
        text " * Copyright © " <> ppLexeme from <> char '-' <> ppLexeme to <+>
        ppCommentBody owner
    CopyrightDecl from Nothing owner ->
        text " * Copyright © " <> ppLexeme from <+>
        ppCommentBody owner

    Comment style _ cs end ->
        ppComment style cs end
    CommentSection start decls end ->
        start <$> line <> ppToplevel decls <> line <$> end
    CommentSectionEnd cs ->
        dullyellow $ ppLexeme cs
    Commented c d ->
        c <$> d
    CommentInfo docs ->
        ppCommentInfo docs

    VarExpr var          -> ppLexeme var
    LiteralExpr _ l      -> dullred $ ppLexeme l
    SizeofExpr arg       -> kwSizeof <> parens arg
    SizeofType arg       -> kwSizeof <> parens arg
    BinaryExpr  l o r    -> l <+> ppBinaryOp o <+> r
    AssignExpr  l o r    -> l <+> ppAssignOp o <+> r
    TernaryExpr c t e    -> ppTernaryExpr c t e
    UnaryExpr o e        -> ppUnaryOp o <> e
    ParenExpr e          -> parens e
    FunctionCall c  a    -> ppFunctionCall c a
    ArrayAccess  e  i    -> e <> char '[' <> i <> char ']'
    CastExpr     ty e    -> parens ty <> e
    CompoundExpr    ty e -> parens ty <+> lbrace <> e <> rbrace  -- DEPRECATED
    CompoundLiteral ty e -> parens ty <+> lbrace <> e <> rbrace
    PreprocDefined  n    -> text "defined(" <> ppLexeme n <> char ')'
    InitialiserList l    -> ppInitialiserList l
    PointerAccess e m    -> e <> text "->" <> ppLexeme m
    MemberAccess  e m    -> e <> text "." <> ppLexeme m
    CommentExpr   c e    -> c <+> e
    Ellipsis             -> text "..."

    VarDecl ty name arrs      -> ty <+> ppLexeme name <> hcat arrs
    DeclSpecArray Nothing     -> text "[]"
    DeclSpecArray (Just dim)  -> brackets dim

    TyPointer     ty -> ty <> char '*'
    TyConst       ty -> ty <+> kwConst
    TyUserDefined l  -> dullgreen $ ppLexeme l
    TyStd         l  -> dullgreen $ ppLexeme l
    TyFunc        l  -> dullgreen $ ppLexeme l
    TyStruct      l  -> kwStruct <+> dullgreen (ppLexeme l)

    ExternC decls ->
        dullmagenta (text "#ifdef __cplusplus") <$>
        kwExtern <+> dullred (text "\"C\"") <+> lbrace <$>
        dullmagenta (text "#endif") <$>
        line <>
        ppToplevel decls <$>
        line <>
        dullmagenta (text "#ifdef __cplusplus") <$>
        rbrace <$>
        dullmagenta (text "#endif")

    Group decls -> vcat decls

    MacroParam l -> ppLexeme l

    MacroBodyFunCall e -> e
    MacroBodyStmt body ->
        kwDo <+> body <+> kwWhile <+> text "(0)"

    PreprocScopedDefine def stmts undef ->
        def <$> ppToplevel stmts <$> undef

    PreprocInclude hdr ->
        dullmagenta $ text "#include" <+> ppLexeme hdr
    PreprocDefine name ->
        dullmagenta $ text "#define" <+> ppLexeme name
    PreprocDefineConst name value ->
        dullmagenta $ text "#define" <+> ppLexeme name <+> value
    PreprocDefineMacro name params body ->
        ppMacroBody $ text "#define" <+> ppLexeme name <> ppParamList params <+> body
    PreprocUndef name ->
        dullmagenta $ text "#undef" <+> ppLexeme name

    PreprocIf cond decls elseBranch ->
        dullmagenta (text "#if" <+> cond) <$>
        ppToplevel decls <>
        elseBranch <$>
        dullmagenta (text "#endif")
    PreprocIfdef name decls elseBranch ->
        dullmagenta (text "#ifdef" <+> ppLexeme name) <$>
        ppToplevel decls <>
        elseBranch <$>
        dullmagenta (text "#endif  //" <+> ppLexeme name)
    PreprocIfndef name decls elseBranch ->
        dullmagenta (text "#ifndef" <+> ppLexeme name) <$>
        ppToplevel decls <>
        elseBranch <$>
        dullmagenta (text "#endif  //" <+> ppLexeme name)
    PreprocElse [] -> empty
    PreprocElse decls ->
        linebreak <>
        dullmagenta (text "#else") <$>
        ppToplevel decls
    PreprocElif cond decls elseBranch ->
        hardline <>
        dullmagenta (text "#elif") <+> cond <$>
        ppToplevel decls <>
        elseBranch

    AttrPrintf fmt ellipsis fun ->
        kwGnuPrintf <> ppIntList [fmt, ellipsis] <$> fun
    CallbackDecl ty name ->
        ppLexeme ty <+> ppLexeme name
    FunctionPrototype ty name params ->
        ppFunctionPrototype ty name params
    FunctionDecl scope proto ->
        ppScope scope <> proto <> semi
    FunctionDefn scope proto body ->
        ppScope scope <> proto <+> body

    MemberDecl decl Nothing ->
        decl <> semi
    MemberDecl decl (Just size) ->
        decl <+> colon <+> ppLexeme size <> semi

    AggregateDecl struct -> struct <> semi
    Struct name members ->
        nest indentWidth (
            kwStruct <+> ppLexeme name <+> lbrace <$>
            vcat members
        ) <$> rbrace
    Union name members ->
        nest indentWidth (
            kwUnion <+> ppLexeme name <+> lbrace <$>
            vcat members
        ) <$> rbrace
    Typedef ty tyname ->
        kwTypedef <+> ty <+> dullgreen (ppLexeme tyname) <> semi
    TypedefFunction proto ->
        kwTypedef <+> proto <> semi

    ConstDecl ty name ->
        kwExtern <+> kwConst <+> ty <+> ppLexeme name <> semi
    ConstDefn scope ty name value ->
        ppScope scope <> kwConst <+>
        ty <+> ppLexeme name <+> equals <+> value <> semi

    Enumerator name  Nothing -> ppLexeme name <> comma
    Enumerator name (Just value) ->
        ppLexeme name <+> equals <+> value <> comma

    EnumConsts Nothing enums ->
        nest indentWidth (
            kwEnum <+> lbrace <$>
            vcat enums
        ) <$> text "};"
    EnumConsts (Just name) enums ->
        nest indentWidth (
            kwEnum <+> ppLexeme name <+> lbrace <$>
            vcat enums
        ) <$> text "};"
    EnumDecl name enums ty ->
        nest indentWidth (
            kwTypedef <+> kwEnum <+> dullgreen (ppLexeme name) <+> lbrace <$>
            vcat enums
        ) <$> rbrace <+> dullgreen (ppLexeme ty) <> semi

    NonNull [] [] f ->
        kwNonNull <> text "()" <$> f
    NonNull nonnull [] f ->
        kwNonNull <> ppIntList nonnull <$> f
    NonNull [] nullable f ->
        kwNullable <> ppIntList nullable <$> f
    NonNull nonnull nullable f ->
        kwNonNull <> ppIntList nonnull <+> kwNullable <> ppIntList nullable <$> f

    -- Statements
    VarDeclStmt decl Nothing      -> decl <> semi
    VarDeclStmt decl (Just initr) -> decl <+> equals <+> initr <> semi
    Return Nothing                -> kwReturn <> semi
    Return (Just e)               -> kwReturn <+> e <> semi
    Continue                      -> kwContinue <> semi
    Break                         -> kwBreak <> semi
    IfStmt cond t e               -> ppIfStmt cond t e
    ForStmt i c n body            -> ppForStmt i c n body
    Default s                     -> kwDefault <> colon <+> s
    Label l s                     -> indent (-99) (line <> ppLexeme l <> colon) <$> s
    ExprStmt e                    -> e <> semi
    Goto l                        -> kwGoto <+> ppLexeme l <> semi
    Case e s                      -> kwCase <+> e <> colon <+> s
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

renderS :: Doc -> String
renderS = flip displayS "" . renderSmart 1 120

render :: Doc -> Text
render = Text.pack . renderS

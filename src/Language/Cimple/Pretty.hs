{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
module Language.Cimple.Pretty
    ( plain
    , render
    , renderSmart
    , ppTranslationUnit
    , ppNode
    , ppNodeF
    , showNode
    , showNodePlain
    ) where

import           Data.Fix                      (foldFix)
import qualified Data.List.Split               as List
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import           Language.Cimple               (AssignOp (..), BinaryOp (..),
                                                Comment, CommentF (..),
                                                CommentStyle (..), Lexeme (..),
                                                LexemeClass (..), Node,
                                                NodeF (..), Nullability (..),
                                                Scope (..), UnaryOp (..),
                                                lexemeLine, lexemeText)
import           Language.Cimple.PrettyColor   (black, blue, cyan, dullcyan,
                                                dullgreen, dullmagenta, dullred,
                                                dullyellow, underline)
import           Language.Cimple.PrettyComment (ppCommentInfo)
import           Language.Cimple.PrettyCommon
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)

indentWidth :: Int
indentWidth = 2

ppScope :: Scope -> Doc AnsiStyle
ppScope = \case
    Global -> mempty
    Static -> kwStatic <> space
    Local  -> mempty

ppNullability :: Nullability -> Doc AnsiStyle
ppNullability = \case
    NullabilityUnspecified -> mempty
    Nullable               -> kwNullable
    Nonnull                -> kwNonnull

ppCommentStart :: CommentStyle -> Doc AnsiStyle
ppCommentStart = dullyellow . \case
    Block   -> pretty "/***"
    Doxygen -> pretty "/**"
    Section -> pretty "/** @{"
    Regular -> pretty "/*"
    Ignore  -> pretty "//!TOKSTYLE-"

ppCommentBody :: [Lexeme (Doc AnsiStyle)] -> Doc AnsiStyle
ppCommentBody body = vsep . prefixStars . map (hcat . map ppWord . spaceWords) . groupLines $ body
  where
    -- If the "*/" is on a separate line, don't add an additional "*" before
    -- it. If "*/" is on the same line, then do add a "*" prefix on the last line.
    stars =
        case reverse body of
          e:c:_ | lexemeLine e > lexemeLine c -> 2
          _                                   -> 1
    prefixStars xs = zipWith (<>) (mempty : replicate (length xs - stars) cmtPrefix ++ [mempty]) xs
    groupLines = List.splitWhen $ \case
        L _ PpNewline _ -> True
        _               -> False

    spaceWords = \case
        (L c p s:ws) -> L c p s:continue ws
        []           -> []
      where
        continue [] = []
        continue (w@(L _ CmtEnd _):ws) = w:continue ws
        continue (w@(L _ PctComma _):ws) = w:continue ws
        continue (w@(L _ PctPeriod _):ws) = w:continue ws
        continue (w@(L _ PctEMark _):ws) = w:continue ws
        continue (w@(L _ PctQMark _):ws) = w:continue ws
        continue (w@(L _ PctRParen _):ws) = w:continue ws
        continue [w@(L c p s), end@(L _ CmtEnd _)] | lexemeLine w == lexemeLine end = [L c p s, end]
        continue (L c PctLParen s:w:ws) = L c PctLParen s:w:continue ws
        continue (L c p s:ws) = L c p s:continue ws

ppWord :: Lexeme (Doc AnsiStyle) -> Doc AnsiStyle
ppWord l@(L _ CmtSpace   _) = lexemeText l
ppWord l@(L _ CmtCommand _) = dullcyan   $ lexemeText l
ppWord l@(L _ _          _) = dullyellow $ lexemeText l

ppComment :: CommentStyle -> [Lexeme (Doc AnsiStyle)] -> Lexeme (Doc AnsiStyle) -> Doc AnsiStyle
ppComment Ignore cs _ =
    ppCommentStart Ignore <> hcat (map ppWord cs) <> dullyellow (pretty "//!TOKSTYLE+" <> line)
ppComment style cs end =
    nest 1 $ ppCommentStart style <> ppCommentBody (cs ++ [end])

ppInitialiserList :: [Doc AnsiStyle] -> Doc AnsiStyle
ppInitialiserList l = lbrace <+> commaSep l <+> rbrace

ppParamList :: [Doc AnsiStyle] -> Doc AnsiStyle
ppParamList = parens . indent 0 . commaSep

ppFunctionPrototype
    :: Pretty a
    => Doc AnsiStyle
    -> Lexeme a
    -> [Doc AnsiStyle]
    -> Doc AnsiStyle
ppFunctionPrototype ty name params =
    ty <+> ppLexeme name <> ppParamList params

ppFunctionCall :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
ppFunctionCall callee args =
    callee <> ppParamList args

ppIfStmt
    :: Doc AnsiStyle
    -> Doc AnsiStyle
    -> Maybe (Doc AnsiStyle)
    -> Doc AnsiStyle
ppIfStmt cond t Nothing =
    kwIf <+> parens cond <+> t
ppIfStmt cond t (Just e) =
    kwIf <+> parens cond <+> t <+> kwElse <+> e

ppForStmt
    :: Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
ppForStmt i c n body =
    kwFor <+> parens (i <+> c <> semi <+> n) <+> body

ppWhileStmt
    :: Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
ppWhileStmt c body =
    kwWhile <+> parens c <+> body

ppDoWhileStmt
    :: Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
ppDoWhileStmt body c =
    kwDo <+> body <+> kwWhile <+> parens c <> semi

ppSwitchStmt
    :: Doc AnsiStyle
    -> [Doc AnsiStyle]
    -> Doc AnsiStyle
ppSwitchStmt c body =
    nest indentWidth (
        kwSwitch <+> parens c <+> lbrace <> line <>
        vcat body
    ) <> line <> rbrace

ppVLA :: Pretty a => Doc AnsiStyle -> Lexeme a -> Doc AnsiStyle -> Doc AnsiStyle
ppVLA ty n sz =
    pretty "VLA("
        <> ty
        <> pretty ", "
        <> ppLexeme n
        <> pretty ", "
        <> sz
        <> pretty ");"

ppCompoundStmt :: [Doc AnsiStyle] -> Doc AnsiStyle
ppCompoundStmt body =
    nest indentWidth (
        lbrace <> line <>
        ppToplevel body
    ) <> line <> rbrace

ppTernaryExpr
    :: Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
ppTernaryExpr c t e =
    c <+> pretty '?' <+> t <+> colon <+> e

ppLicenseDecl :: Pretty a => Lexeme a -> [Doc AnsiStyle] -> Doc AnsiStyle
ppLicenseDecl l cs =
    dullyellow $ ppCommentStart Regular <+> pretty "SPDX-License-Identifier: " <> ppLexeme l <> line <>
    vcat (map dullyellow cs) <> line <>
    dullyellow (pretty " */")

ppIntList :: Pretty a => [Lexeme a] -> Doc AnsiStyle
ppIntList = parens . commaSep . map (dullred . ppLexeme)

ppMacroBody :: Doc AnsiStyle -> Doc AnsiStyle
ppMacroBody =
    vcat
    . map dullmagenta
    . punctuate (pretty " \\")
    . map pretty
    . List.splitOn "\n"
    . renderS
    . plain

ppNode :: Pretty a => Node (Lexeme a) -> Doc AnsiStyle
ppNode = foldFix ppNodeF

ppNodeF :: Pretty a => NodeF (Lexeme a) (Doc AnsiStyle) -> Doc AnsiStyle
ppNodeF = \case
    StaticAssert cond msg ->
        kwStaticAssert <> parens (cond <> comma <+> dullred (ppLexeme msg)) <> semi

    LicenseDecl l cs -> ppLicenseDecl l cs
    CopyrightDecl from (Just to) owner ->
        pretty " * Copyright © " <> ppLexeme from <> pretty '-' <> ppLexeme to <+>
        ppCommentBody (fmap pretty <$> owner)
    CopyrightDecl from Nothing owner ->
        pretty " * Copyright © " <> ppLexeme from <+>
        ppCommentBody (fmap pretty <$> owner)

    Comment style _ cs (L l c _) ->
        ppComment style (fmap pretty <$> cs) (L l c (pretty "*/"))
    CommentSection start decls end ->
        start <> line <> line <> ppToplevel decls <> line <> line <> end
    CommentSectionEnd cs ->
        dullyellow $ ppLexeme cs
    Commented c d ->
        c <> line <> d
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
    ArrayAccess  e  i    -> e <> pretty '[' <> i <> pretty ']'
    CastExpr     ty e    -> parens ty <> e
    CompoundExpr    ty e -> parens ty <+> lbrace <> e <> rbrace  -- DEPRECATED
    CompoundLiteral ty e -> parens ty <+> lbrace <> e <> rbrace
    PreprocDefined  n    -> pretty "defined(" <> ppLexeme n <> pretty ')'
    InitialiserList l    -> ppInitialiserList l
    PointerAccess e m    -> e <> pretty "->" <> ppLexeme m
    MemberAccess  e m    -> e <> pretty "." <> ppLexeme m
    CommentExpr   c e    -> c <+> e
    Ellipsis             -> pretty "..."

    VarDecl ty name arrs      -> ty <+> ppLexeme name <> hcat arrs
    DeclSpecArray Nothing     -> pretty "[]"
    DeclSpecArray (Just dim)  -> brackets dim
    ArrayDim NullabilityUnspecified size -> size
    ArrayDim nullability size -> ppNullability nullability <+> size

    TyBitwise     ty -> kwBitwise <+> ty
    TyForce       ty -> kwForce <+> ty
    TyPointer     ty -> ty <> pretty '*'
    TyConst       ty -> ty <+> kwConst
    TyNonnull     ty -> ty <+> kwNonnull
    TyNullable    ty -> ty <+> kwNullable
    TyOwner       ty -> ty <+> kwOwner
    TyUserDefined l  -> dullgreen $ ppLexeme l
    TyStd         l  -> dullgreen $ ppLexeme l
    TyFunc        l  -> dullgreen $ ppLexeme l
    TyStruct      l  -> kwStruct <+> dullgreen (ppLexeme l)
    TyUnion       l  -> kwUnion <+> dullgreen (ppLexeme l)

    ExternC decls ->
        dullmagenta (pretty "#ifdef __cplusplus") <> line <>
        kwExtern <+> dullred (pretty "\"C\"") <+> lbrace <> line <>
        dullmagenta (pretty "#endif") <> line <>
        line <>
        ppToplevel decls <> line <>
        line <>
        dullmagenta (pretty "#ifdef __cplusplus") <> line <>
        rbrace <+> pretty "/* extern \"C\" */" <> line <>
        dullmagenta (pretty "#endif")

    Group decls -> vcat decls

    MacroParam l -> ppLexeme l

    MacroBodyFunCall e -> e
    MacroBodyStmt body ->
        kwDo <+> body <+> kwWhile <+> pretty "(0)"

    PreprocScopedDefine def stmts undef ->
        def <> line <> ppToplevel stmts <> line <> undef

    PreprocInclude hdr ->
        dullmagenta $ pretty "#include" <+> ppLexeme hdr
    PreprocDefine name ->
        dullmagenta $ pretty "#define" <+> ppLexeme name
    PreprocDefineConst name value ->
        dullmagenta $ pretty "#define" <+> ppLexeme name <+> value
    PreprocDefineMacro name params body ->
        ppMacroBody $ pretty "#define" <+> ppLexeme name <> ppParamList params <+> body
    PreprocUndef name ->
        dullmagenta $ pretty "#undef" <+> ppLexeme name

    PreprocIf cond decls elseBranch ->
        dullmagenta (pretty "#if" <+> cond) <> line <>
        ppToplevel decls <>
        elseBranch <> line <>
        dullmagenta (pretty "#endif  /*" <+> cond <+> pretty "*/")
    PreprocIfdef name decls elseBranch ->
        dullmagenta (pretty "#ifdef" <+> ppLexeme name) <> line <>
        ppToplevel decls <>
        elseBranch <> line <>
        dullmagenta (pretty "#endif  /*" <+> ppLexeme name <+> pretty "*/")
    PreprocIfndef name decls elseBranch ->
        dullmagenta (pretty "#ifndef" <+> ppLexeme name) <> line <>
        ppToplevel decls <>
        elseBranch <> line <>
        dullmagenta (pretty "#endif  /*" <+> ppLexeme name <+> pretty "*/")
    PreprocElse [] -> mempty
    PreprocElse decls ->
        line <>
        dullmagenta (pretty "#else") <> line <>
        ppToplevel decls
    PreprocElif cond decls elseBranch ->
        hardline <>
        dullmagenta (pretty "#elif") <+> cond <> line <>
        ppToplevel decls <>
        elseBranch

    AttrPrintf fmt ellipsis fun ->
        kwGnuPrintf <> ppIntList [fmt, ellipsis] <> line <> fun
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
            kwStruct <+> ppLexeme name <+> lbrace <> line <>
            vcat members
        ) <> line <> rbrace
    Union name members ->
        nest indentWidth (
            kwUnion <+> ppLexeme name <+> lbrace <> line <>
            vcat members
        ) <> line <> rbrace
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
            kwEnum <+> lbrace <> line <>
            vcat enums
        ) <> line <> pretty "};"
    EnumConsts (Just name) enums ->
        nest indentWidth (
            kwEnum <+> ppLexeme name <+> lbrace <> line <>
            vcat enums
        ) <> line <> pretty "};"
    EnumDecl name enums ty ->
        nest indentWidth (
            kwTypedef <+> kwEnum <+> dullgreen (ppLexeme name) <+> lbrace <> line <>
            vcat enums
        ) <> line <> rbrace <+> dullgreen (ppLexeme ty) <> semi

    NonNull [] [] f ->
        kwNonnull <> pretty "()" <> line <> f
    NonNull nonnull [] f ->
        kwNonnull <> ppIntList nonnull <> line <> f
    NonNull [] nullable f ->
        kwNullable <> ppIntList nullable <> line <> f
    NonNull nonnull nullable f ->
        kwNonnull <> ppIntList nonnull <+> kwNullable <> ppIntList nullable <> line <> f

    NonNullParam p ->
        kwNonnull <> pretty "()" <+> p
    NullableParam p ->
        kwNullable <> pretty "()" <+> p

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
    Label l s                     -> indent (-99) (line <> ppLexeme l <> colon) <> line <> s
    ExprStmt e                    -> e <> semi
    Goto l                        -> kwGoto <+> ppLexeme l <> semi
    Case e s                      -> kwCase <+> e <> colon <+> s
    WhileStmt c body              -> ppWhileStmt c body
    DoWhileStmt body c            -> ppDoWhileStmt body c
    SwitchStmt c body             -> ppSwitchStmt c body
    CompoundStmt body             -> ppCompoundStmt body
    VLA ty n sz                   -> ppVLA ty n sz

ppToplevel :: [Doc AnsiStyle] -> Doc AnsiStyle
ppToplevel = vcat . punctuate line

ppTranslationUnit :: Pretty a => [Node (Lexeme a)] -> Doc AnsiStyle
ppTranslationUnit decls = (ppToplevel . map ppNode $ decls) <> line

showNode  :: Pretty a => Node (Lexeme a) -> Text
showNode = render . ppNode

showNodePlain :: Pretty a => Node (Lexeme a) -> Text
showNodePlain = render . plain . ppNode

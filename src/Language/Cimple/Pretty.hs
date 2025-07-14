{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
module Language.Cimple.Pretty
    ( plain
    , render
    , ppTranslationUnit
    , showNode
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
                                                NodeF (..), Scope (..),
                                                UnaryOp (..), lexemeLine,
                                                lexemeText)
import           Language.Cimple.PrettyColor   (black, blue, cyan, dullcyan,
                                                dullgreen, dullmagenta, dullred,
                                                dullyellow, underline)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as Term

indentWidth :: Int
indentWidth = 2

kwBitwise       = dullgreen $ pretty "bitwise"
kwBreak         = dullred   $ pretty "break"
kwCase          = dullred   $ pretty "case"
kwConst         = dullgreen $ pretty "const"
kwContinue      = dullred   $ pretty "continue"
kwDefault       = dullred   $ pretty "default"
kwDo            = dullred   $ pretty "do"
kwElse          = dullred   $ pretty "else"
kwEnum          = dullgreen $ pretty "enum"
kwExtern        = dullgreen $ pretty "extern"
kwFor           = dullred   $ pretty "for"
kwForce         = dullgreen $ pretty "force"
kwGnuPrintf     = dullgreen $ pretty "GNU_PRINTF"
kwGoto          = dullred   $ pretty "goto"
kwIf            = dullred   $ pretty "if"
kwNonnull       = dullgreen $ pretty "_Nonnull"
kwNullable      = dullgreen $ pretty "_Nullable"
kwOwner         = dullgreen $ pretty "owner"
kwReturn        = dullred   $ pretty "return"
kwSizeof        = dullred   $ pretty "sizeof"
kwStaticAssert  = dullred   $ pretty "static_assert"
kwStatic        = dullgreen $ pretty "static"
kwStruct        = dullgreen $ pretty "struct"
kwSwitch        = dullred   $ pretty "switch"
kwTypedef       = dullgreen $ pretty "typedef"
kwUnion         = dullgreen $ pretty "union"
kwWhile         = dullred   $ pretty "while"

kwDocAttention  = dullcyan $ pretty "@attention"
kwDocBrief      = dullcyan $ pretty "@brief"
kwDocDeprecated = dullcyan $ pretty "@deprecated"
kwDocExtends    = dullcyan $ pretty "@extends"
kwDocImplements = dullcyan $ pretty "@implements"
kwDocParam      = dullcyan $ pretty "@param"
kwDocPrivate    = dullcyan $ pretty "@private"
kwDocRef        = dullcyan $ pretty "@ref"
kwDocReturn     = dullcyan $ pretty "@return"
kwDocRetval     = dullcyan $ pretty "@retval"
kwDocP          = dullcyan $ pretty "@p"
kwDocSee        = dullcyan $ pretty "@see"

cmtPrefix :: Doc AnsiStyle
cmtPrefix = dullyellow (pretty '*')

ppText :: Text -> Doc AnsiStyle
ppText = pretty . Text.unpack

ppLexeme :: Lexeme Text -> Doc AnsiStyle
ppLexeme = ppText . lexemeText

commaSep :: [Doc AnsiStyle] -> Doc AnsiStyle
commaSep = hsep . punctuate comma

ppScope :: Scope -> Doc AnsiStyle
ppScope = \case
    Global -> mempty
    Static -> kwStatic <> space

ppAssignOp :: AssignOp -> Doc AnsiStyle
ppAssignOp = \case
    AopEq     -> equals
    AopMul    -> pretty "*="
    AopDiv    -> pretty "/="
    AopPlus   -> pretty "+="
    AopMinus  -> pretty "-="
    AopBitAnd -> pretty "&="
    AopBitOr  -> pretty "|="
    AopBitXor -> pretty "^="
    AopMod    -> pretty "%="
    AopLsh    -> pretty ">>="
    AopRsh    -> pretty "<<="

ppBinaryOp :: BinaryOp -> Doc AnsiStyle
ppBinaryOp = \case
    BopNe     -> pretty "!="
    BopEq     -> pretty "=="
    BopOr     -> pretty "||"
    BopBitXor -> pretty '^'
    BopBitOr  -> pretty '|'
    BopAnd    -> pretty "&&"
    BopBitAnd -> pretty '&'
    BopDiv    -> pretty '/'
    BopMul    -> pretty '*'
    BopMod    -> pretty '%'
    BopPlus   -> pretty '+'
    BopMinus  -> pretty '-'
    BopLt     -> pretty '<'
    BopLe     -> pretty "<="
    BopLsh    -> pretty "<<"
    BopGt     -> pretty '>'
    BopGe     -> pretty ">="
    BopRsh    -> pretty ">>"

ppUnaryOp :: UnaryOp -> Doc AnsiStyle
ppUnaryOp = \case
    UopNot     -> pretty '!'
    UopNeg     -> pretty '~'
    UopMinus   -> pretty '-'
    UopAddress -> pretty '&'
    UopDeref   -> pretty '*'
    UopIncr    -> pretty "++"
    UopDecr    -> pretty "--"

ppCommentStart :: CommentStyle -> Doc AnsiStyle
ppCommentStart = dullyellow . \case
    Block   -> pretty "/***"
    Doxygen -> pretty "/**"
    Section -> pretty "/** @{"
    Regular -> pretty "/*"
    Ignore  -> pretty "//!TOKSTYLE-"

ppCommentBody :: [Lexeme Text] -> Doc AnsiStyle
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
        (L c p s:ws) -> L c p (tSpace<>s):continue ws
        []           -> []
      where
        continue [] = []
        continue (w@(L _ CmtEnd _):ws) = w:continue ws
        continue (w@(L _ PctComma _):ws) = w:continue ws
        continue (w@(L _ PctPeriod _):ws) = w:continue ws
        continue (w@(L _ PctEMark _):ws) = w:continue ws
        continue (w@(L _ PctQMark _):ws) = w:continue ws
        continue (w@(L _ PctRParen _):ws) = w:continue ws
        continue [w@(L c p s), end@(L _ CmtEnd _)] | lexemeLine w == lexemeLine end = [L c p (tSpace<>s<>tSpace), end]
        continue (L c PctLParen s:w:ws) = L c PctLParen (tSpace<>s):w:continue ws
        continue (L c p s:ws) = L c p (tSpace<>s):continue ws

        tSpace :: Text
        tSpace = Text.pack " "

ppWord (L _ CmtIndent  _) = mempty
ppWord (L _ CmtCommand t) = dullcyan   $ ppText t
ppWord (L _ _          t) = dullyellow $ ppText t

ppComment :: CommentStyle -> [Lexeme Text] -> Lexeme Text -> Doc AnsiStyle
ppComment Ignore cs _ =
    ppCommentStart Ignore <> hcat (map ppWord cs) <> dullyellow (pretty "//!TOKSTYLE+" <> line)
ppComment style cs (L l c _) =
    nest 1 $ ppCommentStart style <> ppCommentBody (cs ++ [L l c (Text.pack "*/")])

ppInitialiserList :: [Doc AnsiStyle] -> Doc AnsiStyle
ppInitialiserList l = lbrace <+> commaSep l <+> rbrace

ppParamList :: [Doc AnsiStyle] -> Doc AnsiStyle
ppParamList = parens . indent 0 . commaSep

ppFunctionPrototype
    :: Doc AnsiStyle
    -> Lexeme Text
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
        kwSwitch <+> parens c <+> lbrace <$$>
        vcat body
    ) <$$> rbrace

ppVLA :: Doc AnsiStyle -> Lexeme Text -> Doc AnsiStyle -> Doc AnsiStyle
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
        lbrace <$$>
        ppToplevel body
    ) <$$> rbrace

ppTernaryExpr
    :: Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
    -> Doc AnsiStyle
ppTernaryExpr c t e =
    c <+> pretty '?' <+> t <+> colon <+> e

ppLicenseDecl :: Lexeme Text -> [Doc AnsiStyle] -> Doc AnsiStyle
ppLicenseDecl l cs =
    dullyellow $ ppCommentStart Regular <+> pretty "SPDX-License-Identifier: " <> ppLexeme l <$$>
    vcat (map dullyellow cs) <$$>
    dullyellow (pretty " */")

ppIntList :: [Lexeme Text] -> Doc AnsiStyle
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

plain :: Doc ann -> Doc xxx
plain = unAnnotate

ppVerbatimComment :: Doc AnsiStyle -> Doc AnsiStyle
ppVerbatimComment =
    vcat
    . map dullyellow
    . zipWith (<>) (mempty : repeat (pretty " * "))
    . map pretty
    . List.splitOn "\n"
    . renderS
    . plain

ppCodeBody :: [Doc AnsiStyle] -> Doc AnsiStyle
ppCodeBody =
    vcat
    . zipWith (<>) (mempty : commentStart " *"  )
    . map pretty
    . List.splitOn "\n"
    . renderS
    . plain
    . hcat

commentStart :: String -> [Doc AnsiStyle]
commentStart = repeat . dullyellow . pretty

ppCommentInfo :: Comment (Lexeme Text) -> Doc AnsiStyle
ppCommentInfo = foldFix go
  where
  ppBody     = vcat . zipWith (<>) (        commentStart " * "  )
  ppIndented = vcat . zipWith (<>) (mempty : commentStart " *   ")
  ppRef      = underline . cyan . ppLexeme
  ppAttr     = maybe mempty (blue . ppLexeme)

  go :: CommentF (Lexeme Text) (Doc AnsiStyle) -> Doc AnsiStyle
  go = dullyellow . \case
    DocComment docs ->
        pretty "/**" <$$>
        ppBody docs <$$>
        dullyellow (pretty " */")
    DocWord w -> ppLexeme w
    DocSentence docs ending -> fillSep docs <> ppLexeme ending
    DocNewline -> mempty

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
    DocExtends feat     -> kwDocExtends    <+> ppLexeme feat
    DocImplements feat  -> kwDocImplements <+> ppLexeme feat
    DocPrivate          -> kwDocPrivate

    DocParagraph docs -> ppIndented docs
    DocLine docs -> fillSep docs
    DocCode begin code end -> ppLexeme begin <> ppCodeBody code <> ppLexeme end
    DocList l -> ppVerbatimComment $ vcat l
    DocOLItem num docs -> ppLexeme num <> pretty '.' <+> nest 3 (fillSep docs)
    DocULItem docs sublist -> pretty '-' <+> nest 2 (vsep $ fillSep docs : sublist)

    DocLParen doc -> lparen <> doc
    DocRParen doc -> doc <> rparen
    DocColon doc -> ppLexeme doc <> pretty ':'
    DocBinaryOp BopMinus l r -> l <>  pretty '-'      <>  r
    DocBinaryOp BopDiv   l r -> l <>  pretty '/'      <>  r
    DocAssignOp op       l r -> l <+> ppAssignOp op <+> r
    DocBinaryOp op       l r -> l <+> ppBinaryOp op <+> r

ppNode :: Node (Lexeme Text) -> Doc AnsiStyle
ppNode = foldFix go
  where
  go :: NodeF (Lexeme Text) (Doc AnsiStyle) -> Doc AnsiStyle
  go = \case
    StaticAssert cond msg ->
        kwStaticAssert <> parens (cond <> comma <+> dullred (ppLexeme msg)) <> semi

    LicenseDecl l cs -> ppLicenseDecl l cs
    CopyrightDecl from (Just to) owner ->
        pretty " * Copyright © " <> ppLexeme from <> pretty '-' <> ppLexeme to <>
        ppCommentBody owner
    CopyrightDecl from Nothing owner ->
        pretty " * Copyright © " <> ppLexeme from <>
        ppCommentBody owner

    Comment style _ cs end ->
        ppComment style cs end
    CommentSection start decls end ->
        start <$$> line <> ppToplevel decls <> line <$$> end
    CommentSectionEnd cs ->
        dullyellow $ ppLexeme cs
    Commented c d ->
        c <$$> d
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
        dullmagenta (pretty "#ifdef __cplusplus") <$$>
        kwExtern <+> dullred (pretty "\"C\"") <+> lbrace <$$>
        dullmagenta (pretty "#endif") <$$>
        line <>
        ppToplevel decls <$$>
        line <>
        dullmagenta (pretty "#ifdef __cplusplus") <$$>
        rbrace <+> pretty "/* extern \"C\" */" <$$>
        dullmagenta (pretty "#endif")

    Group decls -> vcat decls

    MacroParam l -> ppLexeme l

    MacroBodyFunCall e -> e
    MacroBodyStmt body ->
        kwDo <+> body <+> kwWhile <+> pretty "(0)"

    PreprocScopedDefine def stmts undef ->
        def <$$> ppToplevel stmts <$$> undef

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
        dullmagenta (pretty "#if" <+> cond) <$$>
        ppToplevel decls <>
        elseBranch <$$>
        dullmagenta (pretty "#endif  /*" <+> cond <+> pretty "*/")
    PreprocIfdef name decls elseBranch ->
        dullmagenta (pretty "#ifdef" <+> ppLexeme name) <$$>
        ppToplevel decls <>
        elseBranch <$$>
        dullmagenta (pretty "#endif  /*" <+> ppLexeme name <+> pretty "*/")
    PreprocIfndef name decls elseBranch ->
        dullmagenta (pretty "#ifndef" <+> ppLexeme name) <$$>
        ppToplevel decls <>
        elseBranch <$$>
        dullmagenta (pretty "#endif  /*" <+> ppLexeme name <+> pretty "*/")
    PreprocElse [] -> mempty
    PreprocElse decls ->
        line <>
        dullmagenta (pretty "#else") <$$>
        ppToplevel decls
    PreprocElif cond decls elseBranch ->
        hardline <>
        dullmagenta (pretty "#elif") <+> cond <$$>
        ppToplevel decls <>
        elseBranch

    AttrPrintf fmt ellipsis fun ->
        kwGnuPrintf <> ppIntList [fmt, ellipsis] <$$> fun
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
            kwStruct <+> ppLexeme name <+> lbrace <$$>
            vcat members
        ) <$$> rbrace
    Union name members ->
        nest indentWidth (
            kwUnion <+> ppLexeme name <+> lbrace <$$>
            vcat members
        ) <$$> rbrace
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
            kwEnum <+> lbrace <$$>
            vcat enums
        ) <$$> pretty "};"
    EnumConsts (Just name) enums ->
        nest indentWidth (
            kwEnum <+> ppLexeme name <+> lbrace <$$>
            vcat enums
        ) <$$> pretty "};"
    EnumDecl name enums ty ->
        nest indentWidth (
            kwTypedef <+> kwEnum <+> dullgreen (ppLexeme name) <+> lbrace <$$>
            vcat enums
        ) <$$> rbrace <+> dullgreen (ppLexeme ty) <> semi

    NonNull [] [] f ->
        kwNonnull <> pretty "()" <$$> f
    NonNull nonnull [] f ->
        kwNonnull <> ppIntList nonnull <$$> f
    NonNull [] nullable f ->
        kwNullable <> ppIntList nullable <$$> f
    NonNull nonnull nullable f ->
        kwNonnull <> ppIntList nonnull <+> kwNullable <> ppIntList nullable <$$> f

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
    Label l s                     -> indent (-99) (line <> ppLexeme l <> colon) <$$> s
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

ppTranslationUnit :: [Node (Lexeme Text)] -> Doc AnsiStyle
ppTranslationUnit decls = (ppToplevel . map ppNode $ decls) <> line

showNode  :: Node (Lexeme Text) -> Text
showNode = render . ppNode

renderSmart :: Float -> Int -> Doc AnsiStyle -> SimpleDocStream AnsiStyle
renderSmart ribbonFraction widthPerLine
    = layoutSmart LayoutOptions
        { layoutPageWidth = AvailablePerLine widthPerLine (realToFrac ribbonFraction) }

renderS :: Doc AnsiStyle -> String
renderS = Text.unpack . render

render :: Doc AnsiStyle -> Text
render = TL.toStrict . Term.renderLazy . renderSmart 1 120

infixr 5 <$$>
(<$$>) :: Doc a -> Doc a -> Doc a
x <$$> y = x <> line <> y

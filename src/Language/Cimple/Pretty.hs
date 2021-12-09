module Language.Cimple.Pretty (ppTranslationUnit) where

import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (AssignOp (..), BinaryOp (..),
                                               CommentStyle (..), Lexeme (..),
                                               LexemeClass (..), Node (..),
                                               Scope (..), UnaryOp (..),
                                               lexemeText)
import           Prelude                      hiding ((<$>))
import           Text.Groom                   (groom)
import           Text.PrettyPrint.ANSI.Leijen

ppText :: Text -> Doc
ppText = text . Text.unpack

ppLexeme :: Lexeme Text -> Doc
ppLexeme = ppText . lexemeText

ppCommaSep :: (a -> Doc) -> [a] -> Doc
ppCommaSep go = foldr (<>) empty . List.intersperse (text ", ") . map go

ppLineSep :: (a -> Doc) -> [a] -> Doc
ppLineSep go = foldr (<>) empty . List.intersperse linebreak . map go

ppComment :: CommentStyle -> [Node (Lexeme Text)] -> Doc
ppComment style cs =
    nest 1 (ppCommentStyle style <> ppCommentBody cs) <+> text "*/"

ppCommentStyle :: CommentStyle -> Doc
ppCommentStyle Block   = text "/***"
ppCommentStyle Doxygen = text "/**"
ppCommentStyle Regular = text "/*"

ppCommentBody :: [Node (Lexeme Text)] -> Doc
ppCommentBody = go . map unCommentWord
  where
    unCommentWord (CommentWord l) = l
    unCommentWord x               = error $ groom x

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
    ppWord x                        = error $ groom x

ppScope :: Scope -> Doc
ppScope Global = empty
ppScope Static = text "static "

ppType :: Node (Lexeme Text) -> Doc
ppType (TyPointer     ty) = ppType ty <> char '*'
ppType (TyConst       ty) = ppType ty <+> text "const"
ppType (TyUserDefined l ) = ppLexeme l
ppType (TyStd         l ) = ppLexeme l
ppType (TyFunc        l ) = ppLexeme l
ppType (TyStruct      l ) = text "struct" <+> ppLexeme l
ppType (TyVar         l ) = ppLexeme l
ppType x                  = error . groom $ x

ppAssignOp :: AssignOp -> Doc
ppAssignOp op = case op of
    AopEq     -> text "="
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
ppBinaryOp op = case op of
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
ppUnaryOp op = case op of
    UopNot     -> char '!'
    UopNeg     -> char '~'
    UopMinus   -> char '-'
    UopAddress -> char '&'
    UopDeref   -> char '*'
    UopIncr    -> text "++"
    UopDecr    -> text "--"

ppInitialiserList :: [Node (Lexeme Text)] -> Doc
ppInitialiserList l = char '{' <+> ppCommaSep ppExpr l <+> char '}'

ppDeclSpec :: Node (Lexeme Text) -> Doc
ppDeclSpec (DeclSpecVar var        ) = ppLexeme var
ppDeclSpec (DeclSpecArray dspec dim) = ppDeclSpec dspec <> ppDim dim
  where
    ppDim Nothing  = text "[]"
    ppDim (Just x) = char '[' <> ppExpr x <> char ']'
ppDeclSpec x = error $ groom x

ppDeclarator :: Node (Lexeme Text) -> Doc
ppDeclarator (Declarator dspec Nothing) =
    ppDeclSpec dspec
ppDeclarator (Declarator dspec (Just initr)) =
    ppDeclSpec dspec <+> char '=' <+> ppExpr initr
ppDeclarator x = error $ groom x

ppFunctionParamList :: [Node (Lexeme Text)] -> Doc
ppFunctionParamList xs = char '(' <> ppCommaSep go xs <> char ')'
  where
    go (TyStd l@(L _ KwVoid _)) = ppLexeme l
    go (FunctionParam ty dspec) = ppType ty <+> ppDeclSpec dspec
    go Ellipsis                 = text "..."
    go x                        = error $ groom x

ppFunctionPrototype
    :: Node (Lexeme Text)
    -> Lexeme Text
    -> [Node (Lexeme Text)]
    -> Doc
ppFunctionPrototype ty name params =
    ppType ty <+> ppLexeme name <> ppFunctionParamList params

ppWithError :: Maybe (Node (Lexeme Text)) -> Doc
ppWithError Nothing = char ';'
ppWithError (Just (ErrorFor name)) =
    text " with error for" <+> ppLexeme name <> char ';'
ppWithError (Just (ErrorList errs)) =
    nest 2 (
        text " with error" <+> char '{' <$>
        ppEnumeratorList errs
    ) <$> char '}'
ppWithError x = error $ groom x

ppFunctionCall :: Node (Lexeme Text) -> [Node (Lexeme Text)] -> Doc
ppFunctionCall callee args =
    ppExpr callee <> char '(' <> ppCommaSep ppExpr args <> char ')'

ppMacroBody :: Node (Lexeme Text) -> Doc
ppMacroBody (MacroBodyFunCall e@FunctionCall{}) = ppExpr e
ppMacroBody (MacroBodyStmt body) =
    nest 2 (
        text "do {" <$>
        ppStmtList body
    ) <$> text "} while (0)"
ppMacroBody x                                   = error $ groom x

ppMacroParam :: Node (Lexeme Text) -> Doc
ppMacroParam (MacroParam l) = ppLexeme l
ppMacroParam Ellipsis       = text "..."
ppMacroParam x              = error $ groom x

ppMacroParamList :: [Node (Lexeme Text)] -> Doc
ppMacroParamList xs = char '(' <> ppCommaSep ppMacroParam xs <> char ')'

ppNamespace :: ([a] -> Doc) -> Scope -> Lexeme Text -> [a] -> Doc
ppNamespace pp scope name members =
    nest 2 (
        ppScope scope <>
        text "namespace" <+> ppLexeme name <+> char '{' <$>
        pp members
    ) <$> char '}'

ppEnumerator :: Node (Lexeme Text) -> Doc
ppEnumerator (Comment    style _ cs _ ) = ppComment style cs
ppEnumerator (Enumerator name  Nothing) = ppLexeme name <> char ','
ppEnumerator (Enumerator name (Just value)) =
    ppLexeme name <+> char '=' <+> ppExpr value <> char ','
ppEnumerator (Namespace scope name members) =
    ppNamespace ppEnumeratorList scope name members
ppEnumerator x = error $ groom x

ppEnumeratorList :: [Node (Lexeme Text)] -> Doc
ppEnumeratorList = ppLineSep ppEnumerator

ppMemberDecl :: Node (Lexeme Text) -> Doc
ppMemberDecl = ppDecl

ppMemberDeclList :: [Node (Lexeme Text)] -> Doc
ppMemberDeclList = ppLineSep ppMemberDecl

ppAccessor :: Node (Lexeme Text) -> Doc
ppAccessor (Comment style _ cs _) = ppComment style cs
ppAccessor (Accessor name params errs) =
    ppLexeme name <> ppFunctionParamList params <> ppWithError errs
ppAccessor x = error $ groom x

ppAccessorList :: [Node (Lexeme Text)] -> Doc
ppAccessorList = ppLineSep ppAccessor

ppEventType :: Node (Lexeme Text) -> Doc
ppEventType (Commented (Comment style _ cs _) ty) =
    ppComment style cs <$> ppEventType ty
ppEventType (EventParams params) =
    text "typedef void" <> ppFunctionParamList params
ppEventType x = error $ groom x

ppTypeParams :: [Node (Lexeme Text)] -> Doc
ppTypeParams [] = empty
ppTypeParams xs = char '<' <> ppCommaSep pp xs <> char '>'
  where
    pp (TyVar x) = ppLexeme x
    pp x         = error $ groom x

ppCompoundStmt :: [Node (Lexeme Text)] -> Doc
ppCompoundStmt body =
    nest 2 (
        char '{' <$>
        ppStmtList body
    ) <$> char '}'

ppStmtList :: [Node (Lexeme Text)] -> Doc
ppStmtList = ppLineSep ppDecl

ppIfStmt
    :: Node (Lexeme Text)
    -> [Node (Lexeme Text)]
    -> Maybe (Node (Lexeme Text))
    -> Doc
ppIfStmt cond t Nothing =
    nest 2 (
        text "if (" <> ppExpr cond <> text ") {" <$>
        ppStmtList t
    ) <$> char '}'
ppIfStmt cond t (Just e) =
    nest 2 (
        text "if (" <> ppExpr cond <> text ") {" <$>
        ppStmtList t
    ) <$> nest 2 (char '}' <> text " else " <> ppDecl e)

ppForStmt
    :: Node (Lexeme Text)
    -> Node (Lexeme Text)
    -> Node (Lexeme Text)
    -> [Node (Lexeme Text)]
    -> Doc
ppForStmt i c n body =
    nest 2 (
        text "for ("
        <> ppDecl i
        <+> ppExpr c <> char ';'
        <+> ppExpr n
        <> text ") {" <$>
        ppStmtList body
    ) <$> char '}'

ppWhileStmt
    :: Node (Lexeme Text)
    -> [Node (Lexeme Text)]
    -> Doc
ppWhileStmt c body =
    nest 2 (
        text "while ("
        <> ppExpr c
        <> text ") {" <$>
        ppStmtList body
    ) <$> char '}'

ppDoWhileStmt
    :: [Node (Lexeme Text)]
    -> Node (Lexeme Text)
    -> Doc
ppDoWhileStmt body c =
    nest 2 (
        text "do ("
        <> text ") {" <$>
        ppStmtList body
    ) <$> text "} while (" <> ppExpr c <> char ')'

ppSwitchStmt
    :: Node (Lexeme Text)
    -> [Node (Lexeme Text)]
    -> Doc
ppSwitchStmt c body =
    nest 2 (
        text "switch ("
        <> ppExpr c
        <> text ") {" <$>
        ppStmtList body
    ) <$> char '}'

ppExpr :: Node (Lexeme Text) -> Doc
ppExpr expr = case expr of
    -- Expressions
    VarExpr var       -> ppLexeme var
    LiteralExpr _ l   -> ppLexeme l
    SizeofExpr arg    -> text "sizeof(" <> ppExpr arg <> char ')'
    SizeofType arg    -> text "sizeof(" <> ppType arg <> char ')'
    BinaryExpr  l o r -> ppExpr l <+> ppBinaryOp o <+> ppExpr r
    AssignExpr  l o r -> ppExpr l <+> ppAssignOp o <+> ppExpr r
    TernaryExpr c t e -> ppTernaryExpr c t e
    UnaryExpr o e     -> ppUnaryOp o <> ppExpr e
    ParenExpr e       -> char '(' <> ppExpr e <> char ')'
    FunctionCall c  a -> ppFunctionCall c a
    ArrayAccess  e  i -> ppExpr e <> char '[' <> ppExpr i <> char ']'
    CastExpr     ty e -> char '(' <> ppType ty <> char ')' <> ppExpr e
    PreprocDefined  n -> text "defined(" <> ppLexeme n <> char ')'
    InitialiserList l -> ppInitialiserList l
    PointerAccess e m -> ppExpr e <> text "->" <> ppLexeme m
    MemberAccess  e m -> ppExpr e <> text "." <> ppLexeme m
    CommentExpr   c e -> ppCommentExpr c e

    x                 -> error $ groom x

ppTernaryExpr
    :: Node (Lexeme Text) -> Node (Lexeme Text) -> Node (Lexeme Text) -> Doc
ppTernaryExpr c t e =
    ppExpr c <+> char '?' <+> ppExpr t <+> char ':' <+> ppExpr e

ppCommentExpr :: Node (Lexeme Text) -> Node (Lexeme Text) -> Doc
ppCommentExpr (Comment style _ body _) e =
    ppCommentStyle style <+> ppCommentBody body <+> text "*/" <+> ppExpr e
ppCommentExpr c _ = error $ groom c

ppStmt :: Node (Lexeme Text) -> Doc
ppStmt = ppDecl

ppDeclList :: [Node (Lexeme Text)] -> Doc
ppDeclList = ppLineSep ppDecl

ppDecl :: Node (Lexeme Text) -> Doc
ppDecl decl = case decl of
    PreprocElif cond decls (PreprocElse []) ->
        text "#elif" <+> ppExpr cond <$>
        ppDeclList decls <$>
        text "#endif"
    PreprocElif cond decls elseBranch ->
        text "#elif" <+> ppExpr cond <$>
        ppDeclList decls <$>
        ppDeclList [elseBranch] <$>
        text "#endif"
    PreprocIf cond decls (PreprocElse []) ->
        nest (-100) (text "#if") <+> ppExpr cond <$>
        ppDeclList decls <$>
        text "#endif"
    PreprocIf cond decls elseBranch ->
        text "#if" <+> ppExpr cond <$>
        ppDeclList decls <$>
        ppDeclList [elseBranch] <$>
        text "#endif"
    PreprocIfdef name decls (PreprocElse []) ->
        indent (-2) (text "#ifndef" <+> ppLexeme name <$>
        ppDeclList decls) <$>
        text "#endif"
    PreprocIfdef name decls elseBranch ->
        text "#ifdef" <+> ppLexeme name <$>
        ppDeclList decls <$>
        ppDeclList [elseBranch] <$>
        text "#endif"
    PreprocIfndef name decls (PreprocElse []) ->
        text "#ifndef" <+> ppLexeme name <$>
        ppDeclList decls <$>
        text "#endif"
    PreprocIfndef name decls elseBranch ->
        text "#ifndef" <+> ppLexeme name <$>
        ppDeclList decls <$>
        ppDeclList [elseBranch] <$>
        text "#endif"
    PreprocElse decls ->
        text "#else" <$>
        ppDeclList decls

    PreprocScopedDefine def stmts undef ->
        ppDecl def <$> ppStmtList stmts <$> ppDecl undef

    PreprocInclude hdr ->
        text "#include" <+> ppLexeme hdr
    PreprocDefine name ->
        text "#define" <+> ppLexeme name
    PreprocDefineConst name value ->
        text "#define" <+> ppLexeme name <+> ppExpr value
    PreprocDefineMacro name params body ->
        text "#define" <+> ppLexeme name <> ppMacroParamList params <+> ppMacroBody body
    PreprocUndef name ->
        text "#undef" <+> ppLexeme name

    StaticAssert cond msg ->
        text "static_assert" <+> ppExpr cond <> char ',' <+> ppLexeme msg <> text ");"

    Comment style _ cs _ ->
        ppComment style cs
    CommentBlock cs ->
        ppLexeme cs

    ClassForward name [] ->
        text "class" <+> ppLexeme name <> char ';'
    Class scope name tyvars decls ->
        ppScope scope <>
        nest 2 (
            text "class" <+> ppLexeme name <> ppTypeParams tyvars <+> char '{' <$>
            ppDeclList decls
        ) <$> text "};"

    EnumConsts Nothing enums ->
        nest 2 (
            text "enum" <+> char '{' <$>
            ppEnumeratorList enums
        ) <$> text "};"
    EnumConsts (Just name) enums ->
        nest 2 (
            text "enum" <+> ppLexeme name <+> char '{' <$>
            ppEnumeratorList enums
        ) <$> text "};"
    EnumClass name enums ->
        nest 2 (
            text "enum class" <+> ppLexeme name <+> char '{' <$>
            ppEnumeratorList enums
        ) <$> text "};"
    EnumDecl name enums ty ->
        nest 2 (
            text "typedef enum" <+> ppLexeme name <+> char '{' <$>
            ppEnumeratorList enums
        ) <$> text "} " <> ppLexeme ty <> char ';'

    Namespace scope name decls ->
        ppNamespace ppDeclList scope name decls

    ExternC decls ->
        text "#ifndef __cplusplus" <$>
        text "extern \"C\" {" <$>
        text "#endif" <$>
        ppDeclList decls <$>
        text "#ifndef __cplusplus" <$>
        text "}" <$>
        text "#endif"

    Struct name members ->
        nest 2 (
            text "struct" <+> ppLexeme name <+> char '{' <$>
            ppMemberDeclList members
        ) <$> text "};"
    Typedef (Union name members) tyname ->
        nest 2 (
            text "typedef union" <+> ppLexeme name <+> char '{' <$>
            ppMemberDeclList members
        ) <$> char '}' <+> ppLexeme tyname <> char ';'
    Typedef (Struct name members) tyname ->
        nest 2 (
            text "typedef struct" <+> ppLexeme name <+> char '{' <$>
            ppMemberDeclList members
        ) <$> char '}' <+> ppLexeme tyname <> char ';'
    Typedef ty name ->
        text "typedef" <+> ppType ty <+> ppLexeme name <> char ';'
    TypedefFunction (FunctionPrototype ty name params) ->
        text "typedef" <+>
        ppFunctionPrototype ty name params <>
        char ';'

    MemberDecl ty dspec Nothing ->
        ppType ty <+> ppDeclSpec dspec <> char ';'
    MemberDecl ty dspec (Just size) ->
        ppType ty <+> ppDeclSpec dspec <+> char ':' <+> ppLexeme size <> char ';'

    FunctionDecl scope (FunctionPrototype ty name params) err ->
        ppScope scope <>
        ppFunctionPrototype ty name params <>
        ppWithError err
    FunctionDefn scope (FunctionPrototype ty name params) body ->
        ppScope scope <>
        ppFunctionPrototype ty name params <$>
        ppCompoundStmt body

    ConstDecl ty name ->
        text "extern const" <+> ppType ty <+> ppLexeme name <> char ';'
    ConstDefn scope ty name value ->
        ppScope scope <>
        ppType ty <+> ppLexeme name <+> char '=' <+> ppExpr value <> char ';'

    Event name ty ->
        nest 2 (
            text "event" <+> ppLexeme name <+> char '{' <$>
            ppEventType ty
        ) <$> char '}'

    Property ty dspec accessors ->
        nest 2 (
            ppType ty <+> ppDeclSpec dspec <+> char '{' <$>
            ppAccessorList accessors
        ) <$> char '}'

    ErrorDecl name errs ->
        nest 2 (
            text "error for" <+> ppLexeme name <+> char '{' <$>
            ppEnumeratorList errs
        ) <$> char '}'

    -- Statements
    Continue              -> text "continue;"
    Break                 -> text "break;"
    Return Nothing        -> text "return;"
    Return (Just e)       -> text "return" <+> ppExpr e <> char ';'
    VarDecl ty declr      -> ppType ty <+> ppDeclarator declr <> char ';'
    IfStmt cond t e       -> ppIfStmt cond t e
    ForStmt i c n body    -> ppForStmt i c n body
    Default s             -> text "default:" <+> ppStmt s
    Label l s             -> ppLexeme l <> char ':' <$> ppStmt s
    Goto l                -> text "goto " <> ppLexeme l <> char ';'
    Case        e    s    -> text "case " <> ppExpr e <> char ':' <+> ppStmt s
    WhileStmt   c    body -> ppWhileStmt c body
    DoWhileStmt body c    -> ppDoWhileStmt body c
    SwitchStmt  c    body -> ppSwitchStmt c body
    CompoundStmt body     -> char '{' <$> ppStmtList body <$> char '}'
    VLA ty n sz           -> ppVLA ty n sz

    x                     -> ppExpr x <> char ';'


ppVLA :: Node (Lexeme Text) -> Lexeme Text -> Node (Lexeme Text) -> Doc
ppVLA ty n sz =
    text "VLA("
        <> ppType ty
        <> text ", "
        <> ppLexeme n
        <> text ", "
        <> ppExpr sz
        <> text ");"

ppTranslationUnit :: [Node (Lexeme Text)] -> Doc
ppTranslationUnit decls = ppDeclList decls <> linebreak

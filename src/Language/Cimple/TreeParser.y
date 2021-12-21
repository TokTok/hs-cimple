{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Cimple.TreeParser where

import           Data.Text             (Text)
import           Language.Cimple.AST   (CommentStyle (..), Node (..))
import           Language.Cimple.Lexer (Lexeme)
}

%name parseTranslationUnit TranslationUnit
%name parseDecls Decls
%name parseHeaderBody HeaderBody

%error {parseError}
%errorhandlertype explist
%monad {TreeParser}
%tokentype {TextNode}
%token
    ifndefDefine	{ PreprocIfndef _ body (PreprocElse []) | isDefine body }
    ifdefDefine		{ PreprocIfdef _ body (PreprocElse []) | isDefine body }
    ifDefine		{ PreprocIf _ body (PreprocElse []) | isDefine body }

    ifndefInclude	{ PreprocIfndef{} | isPreproc tk && hasInclude tk }
    ifdefInclude	{ PreprocIfdef{} | isPreproc tk && hasInclude tk }
    ifInclude		{ PreprocIf{} | isPreproc tk && hasInclude tk }

    docComment		{ Comment Doxygen _ _ _ }

    -- Preprocessor
    preprocInclude	{ PreprocInclude{} }
    preprocDefine	{ PreprocDefine{} }
    preprocDefineConst	{ PreprocDefineConst{} }
    preprocDefineMacro	{ PreprocDefineMacro{} }
    preprocIf		{ PreprocIf{} }
    preprocIfdef	{ PreprocIfdef{} }
    preprocIfndef	{ PreprocIfndef{} }
    preprocElse		{ PreprocElse{} }
    preprocElif		{ PreprocElif{} }
    preprocUndef	{ PreprocUndef{} }
    preprocDefined	{ PreprocDefined{} }
    preprocScopedDefine	{ PreprocScopedDefine{} }
    macroBodyStmt	{ MacroBodyStmt{} }
    macroBodyFunCall	{ MacroBodyFunCall{} }
    macroParam		{ MacroParam{} }
    staticAssert	{ StaticAssert{} }
    -- Comments
    licenseDecl		{ LicenseDecl{} }
    copyrightDecl	{ CopyrightDecl{} }
    comment		{ Comment{} }
    commentBlock	{ CommentBlock{} }
    commentWord		{ CommentWord{} }
    commented		{ Commented{} }
    -- Namespace-like blocks
    externC		{ ExternC{} }
    class		{ Class{} }
    namespace		{ Namespace{} }
    -- Statements
    compoundStmt	{ CompoundStmt{} }
    break		{ Break }
    goto		{ Goto{} }
    continue		{ Continue }
    return		{ Return{} }
    switchStmt		{ SwitchStmt{} }
    ifStmt		{ IfStmt{} }
    forStmt		{ ForStmt{} }
    whileStmt		{ WhileStmt{} }
    doWhileStmt		{ DoWhileStmt{} }
    case		{ Case{} }
    default		{ Default{} }
    label		{ Label{} }
    -- Variable declarations
    vLA			{ VLA{} }
    varDecl		{ VarDecl{} }
    declarator		{ Declarator{} }
    declSpecVar		{ DeclSpecVar{} }
    declSpecArray	{ DeclSpecArray{} }
    -- Expressions
    initialiserList	{ InitialiserList{} }
    unaryExpr		{ UnaryExpr{} }
    binaryExpr		{ BinaryExpr{} }
    ternaryExpr		{ TernaryExpr{} }
    assignExpr		{ AssignExpr{} }
    parenExpr		{ ParenExpr{} }
    castExpr		{ CastExpr{} }
    compoundExpr	{ CompoundExpr{} }
    sizeofExpr		{ SizeofExpr{} }
    sizeofType		{ SizeofType{} }
    literalExpr		{ LiteralExpr{} }
    varExpr		{ VarExpr{} }
    memberAccess	{ MemberAccess{} }
    pointerAccess	{ PointerAccess{} }
    arrayAccess		{ ArrayAccess{} }
    functionCall	{ FunctionCall{} }
    commentExpr		{ CommentExpr{} }
    -- Type definitions
    enumClass		{ EnumClass{} }
    enumConsts		{ EnumConsts{} }
    enumDecl		{ EnumDecl{} }
    enumerator		{ Enumerator{} }
    classForward	{ ClassForward{} }
    typedef		{ Typedef{} }
    typedefFunction	{ TypedefFunction{} }
    struct		{ Struct{} }
    union		{ Union{} }
    memberDecl		{ MemberDecl{} }
    tyConst		{ TyConst{} }
    tyPointer		{ TyPointer{} }
    tyStruct		{ TyStruct{} }
    tyFunc		{ TyFunc{} }
    tyStd		{ TyStd{} }
    tyVar		{ TyVar{} }
    tyUserDefined	{ TyUserDefined{} }
    -- Functions
    functionDecl	{ FunctionDecl{} }
    functionDefn	{ FunctionDefn{} }
    functionPrototype	{ FunctionPrototype{} }
    functionParam	{ FunctionParam{} }
    event		{ Event{} }
    eventParams		{ EventParams{} }
    property		{ Property{} }
    accessor		{ Accessor{} }
    errorDecl		{ ErrorDecl{} }
    errorList		{ ErrorList{} }
    errorFor		{ ErrorFor{} }
    ellipsis		{ Ellipsis }
    -- Constants
    constDecl		{ ConstDecl{} }
    constDefn		{ ConstDefn{} }

%%

TranslationUnit :: { [TextNode] }
TranslationUnit
:	licenseDecl FileComment Header				{ [$1, $2, $3] }
|	licenseDecl FileComment Source				{ $1 : $2 : $3 }
|	licenseDecl             Header				{ [$1, $2] }
|	licenseDecl             Source				{ $1 : $2 }

FileComment :: { TextNode }
FileComment
:	comment							{ $1 }
|	docComment						{ $1 }

Header :: { TextNode }
Header
:	preprocIfndef						{% recurse parseHeaderBody $1 }

HeaderBody :: { [TextNode] }
HeaderBody
:	preprocDefine Includes Decls				{ $1 : reverse $2 ++ reverse $3 }

Source :: { [TextNode] }
Source
:	Features preprocInclude Includes Decls			{ $1 ++ [$2] ++ reverse $3 ++ reverse $4 }

Features :: { [TextNode] }
Features
:								{ [] }
|	Features IfDefine					{ $2 : $1 }

IfDefine :: { TextNode }
IfDefine
:	ifndefDefine						{ $1 }
|	ifdefDefine						{ $1 }
|	ifDefine						{ $1 }

Includes :: { [TextNode] }
Includes
:								{ [] }
|	Includes Include					{ $2 : $1 }

Include :: { TextNode }
Include
:	preprocInclude						{ $1 }
|	ifndefInclude						{ $1 }
|	ifdefInclude						{ $1 }
|	ifInclude						{ $1 }

Decls :: { [TextNode] }
Decls
:								{ [] }
|	Decls Decl						{ $2 : $1 }

Decl :: { TextNode }
Decl
:	comment							{ $1 }
|	CommentableDecl						{ $1 }
|	docComment CommentableDecl				{ Commented $1 $2 }

CommentableDecl :: { TextNode }
CommentableDecl
:	functionDecl						{ $1 }
|	functionDefn						{ $1 }
|	struct							{ $1 }
|	typedef							{ $1 }
|	constDecl						{ $1 }
|	constDefn						{ $1 }
|	enumConsts						{ $1 }
|	enumDecl						{ $1 }
|	externC							{% recurse parseDecls $1 }
|	preprocDefine						{ $1 }
|	preprocDefineConst					{ $1 }
|	preprocDefineMacro					{ $1 }
|	preprocIf						{% recurse parseDecls $1 }
|	preprocIfdef						{% recurse parseDecls $1 }
|	preprocIfndef						{% recurse parseDecls $1 }
|	staticAssert						{ $1 }
|	typedefFunction						{ $1 }
|	IfDefine						{ $1 }

{
type TextLexeme = Lexeme Text
type TextNode = Node TextLexeme

newtype TreeParser a = TreeParser { toEither :: Either String a }
    deriving (Functor, Applicative, Monad)

instance MonadFail TreeParser where
    fail = TreeParser . Left


isDefine :: [TextNode] -> Bool
isDefine (PreprocUndef{}:d)     = isDefine d
isDefine [PreprocDefine{}]      = True
isDefine [PreprocDefineConst{}] = True
isDefine _                      = False

isPreproc :: TextNode -> Bool
isPreproc PreprocInclude{}        = True
isPreproc PreprocUndef{}          = True
isPreproc PreprocDefine{}         = True
isPreproc PreprocDefineConst{}    = True
isPreproc (PreprocIf _ td ed)     = all isPreproc td && isPreproc ed
isPreproc (PreprocIfdef _ td ed)  = all isPreproc td && isPreproc ed
isPreproc (PreprocIfndef _ td ed) = all isPreproc td && isPreproc ed
isPreproc (PreprocElse ed)        = all isPreproc ed
isPreproc _                       = False

hasInclude :: TextNode -> Bool
hasInclude PreprocInclude{}        = True
hasInclude (PreprocIf _ td ed)     = any hasInclude td || hasInclude ed
hasInclude (PreprocIfdef _ td ed)  = any hasInclude td || hasInclude ed
hasInclude (PreprocIfndef _ td ed) = any hasInclude td || hasInclude ed
hasInclude (PreprocElse ed)        = any hasInclude ed
hasInclude _                       = False


recurse :: ([TextNode] -> TreeParser [TextNode]) -> TextNode -> TreeParser TextNode
recurse f (ExternC ds)          = ExternC <$> f ds
recurse f (PreprocIf c t e)     = PreprocIf c <$> f t <*> recurse f e
recurse f (PreprocIfdef c t e)  = PreprocIfdef c <$> f t <*> recurse f e
recurse f (PreprocIfndef c t e) = PreprocIfndef c <$> f t <*> recurse f e
recurse f (PreprocIfndef c t e) = PreprocIfndef c <$> f t <*> recurse f e
recurse f (PreprocElif c t e)   = PreprocElif c <$> f t <*> recurse f e
recurse f (PreprocElse [])      = return $ PreprocElse []
recurse f (PreprocElse e)       = PreprocElse <$> f e
recurse _ ns                    = fail $ show ns


parseError :: ([TextNode], [String]) -> TreeParser a
parseError ([], options) =
    fail $ "end of file; expected one of " <> show options
parseError (n:_, options) =
    fail $ show n <> "; expected one of " <> show options
}

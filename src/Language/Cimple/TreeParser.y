{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Cimple.TreeParser
    ( TreeParser
    , parseTranslationUnit
    , toEither
    ) where

import           Data.Fix              (Fix (..))
import           Data.Text             (Text)
import           Language.Cimple.AST   (CommentStyle (..), Node, NodeF (..))
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
    ifndefDefine	{ Fix (PreprocIfndef _ body (Fix (PreprocElse []))) | isDefine body }
    ifdefDefine		{ Fix (PreprocIfdef _ body (Fix (PreprocElse []))) | isDefine body }
    ifDefine		{ Fix (PreprocIf _ body (Fix (PreprocElse []))) | isDefine body }

    ifndefInclude	{ Fix (PreprocIfndef{}) | isPreproc tk && hasInclude tk }
    ifdefInclude	{ Fix (PreprocIfdef{}) | isPreproc tk && hasInclude tk }
    ifInclude		{ Fix (PreprocIf{}) | isPreproc tk && hasInclude tk }

    docComment		{ Fix (Comment Doxygen _ _ _) }

    -- Preprocessor
    preprocInclude	{ Fix (PreprocInclude{}) }
    preprocDefine	{ Fix (PreprocDefine{}) }
    preprocDefineConst	{ Fix (PreprocDefineConst{}) }
    preprocDefineMacro	{ Fix (PreprocDefineMacro{}) }
    preprocIf		{ Fix (PreprocIf{}) }
    preprocIfdef	{ Fix (PreprocIfdef{}) }
    preprocIfndef	{ Fix (PreprocIfndef{}) }
    preprocElse		{ Fix (PreprocElse{}) }
    preprocElif		{ Fix (PreprocElif{}) }
    preprocUndef	{ Fix (PreprocUndef{}) }
    preprocDefined	{ Fix (PreprocDefined{}) }
    preprocScopedDefine	{ Fix (PreprocScopedDefine{}) }
    macroBodyStmt	{ Fix (MacroBodyStmt{}) }
    macroBodyFunCall	{ Fix (MacroBodyFunCall{}) }
    macroParam		{ Fix (MacroParam{}) }
    staticAssert	{ Fix (StaticAssert{}) }
    -- Comments
    licenseDecl		{ Fix (LicenseDecl{}) }
    copyrightDecl	{ Fix (CopyrightDecl{}) }
    comment		{ Fix (Comment{}) }
    commentBlock	{ Fix (CommentBlock{}) }
    commented		{ Fix (Commented{}) }
    -- Namespace-like blocks
    externC		{ Fix (ExternC{}) }
    -- Statements
    compoundStmt	{ Fix (CompoundStmt{}) }
    break		{ Fix (Break) }
    goto		{ Fix (Goto{}) }
    continue		{ Fix (Continue) }
    return		{ Fix (Return{}) }
    switchStmt		{ Fix (SwitchStmt{}) }
    ifStmt		{ Fix (IfStmt{}) }
    forStmt		{ Fix (ForStmt{}) }
    whileStmt		{ Fix (WhileStmt{}) }
    doWhileStmt		{ Fix (DoWhileStmt{}) }
    case		{ Fix (Case{}) }
    default		{ Fix (Default{}) }
    label		{ Fix (Label{}) }
    -- Variable declarations
    vLA			{ Fix (VLA{}) }
    varDecl		{ Fix (VarDecl{}) }
    declarator		{ Fix (Declarator{}) }
    declSpecVar		{ Fix (DeclSpecVar{}) }
    declSpecArray	{ Fix (DeclSpecArray{}) }
    -- Expressions
    initialiserList	{ Fix (InitialiserList{}) }
    unaryExpr		{ Fix (UnaryExpr{}) }
    binaryExpr		{ Fix (BinaryExpr{}) }
    ternaryExpr		{ Fix (TernaryExpr{}) }
    assignExpr		{ Fix (AssignExpr{}) }
    parenExpr		{ Fix (ParenExpr{}) }
    castExpr		{ Fix (CastExpr{}) }
    compoundExpr	{ Fix (CompoundExpr{}) }
    sizeofExpr		{ Fix (SizeofExpr{}) }
    sizeofType		{ Fix (SizeofType{}) }
    literalExpr		{ Fix (LiteralExpr{}) }
    varExpr		{ Fix (VarExpr{}) }
    memberAccess	{ Fix (MemberAccess{}) }
    pointerAccess	{ Fix (PointerAccess{}) }
    arrayAccess		{ Fix (ArrayAccess{}) }
    functionCall	{ Fix (FunctionCall{}) }
    commentExpr		{ Fix (CommentExpr{}) }
    -- Type definitions
    enumConsts		{ Fix (EnumConsts{}) }
    enumDecl		{ Fix (EnumDecl{}) }
    enumerator		{ Fix (Enumerator{}) }
    typedef		{ Fix (Typedef{}) }
    typedefFunction	{ Fix (TypedefFunction{}) }
    struct		{ Fix (Struct{}) }
    union		{ Fix (Union{}) }
    memberDecl		{ Fix (MemberDecl{}) }
    tyConst		{ Fix (TyConst{}) }
    tyPointer		{ Fix (TyPointer{}) }
    tyStruct		{ Fix (TyStruct{}) }
    tyFunc		{ Fix (TyFunc{}) }
    tyStd		{ Fix (TyStd{}) }
    tyUserDefined	{ Fix (TyUserDefined{}) }
    -- Functions
    functionDecl	{ Fix (FunctionDecl{}) }
    functionDefn	{ Fix (FunctionDefn{}) }
    functionPrototype	{ Fix (FunctionPrototype{}) }
    functionParam	{ Fix (FunctionParam{}) }
    ellipsis		{ Fix (Ellipsis) }
    -- Constants
    constDecl		{ Fix (ConstDecl{}) }
    constDefn		{ Fix (ConstDefn{}) }

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
|	docComment CommentableDecl				{ Fix $ Commented $1 $2 }

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
isDefine (Fix PreprocUndef{}:d)     = isDefine d
isDefine [Fix PreprocDefine{}]      = True
isDefine [Fix PreprocDefineConst{}] = True
isDefine _                          = False

isPreproc :: TextNode -> Bool
isPreproc (Fix PreprocInclude{})        = True
isPreproc (Fix PreprocUndef{})          = True
isPreproc (Fix PreprocDefine{})         = True
isPreproc (Fix PreprocDefineConst{})    = True
isPreproc (Fix (PreprocIf _ td ed))     = all isPreproc td && isPreproc ed
isPreproc (Fix (PreprocIfdef _ td ed))  = all isPreproc td && isPreproc ed
isPreproc (Fix (PreprocIfndef _ td ed)) = all isPreproc td && isPreproc ed
isPreproc (Fix (PreprocElse ed))        = all isPreproc ed
isPreproc _                             = False

hasInclude :: TextNode -> Bool
hasInclude (Fix PreprocInclude{})        = True
hasInclude (Fix (PreprocIf _ td ed))     = any hasInclude td || hasInclude ed
hasInclude (Fix (PreprocIfdef _ td ed))  = any hasInclude td || hasInclude ed
hasInclude (Fix (PreprocIfndef _ td ed)) = any hasInclude td || hasInclude ed
hasInclude (Fix (PreprocElse ed))        = any hasInclude ed
hasInclude _                             = False


recurse :: ([TextNode] -> TreeParser [TextNode]) -> TextNode -> TreeParser TextNode
recurse f (Fix (ExternC ds))          = Fix <$> (ExternC <$> f ds)
recurse f (Fix (PreprocIf c t e))     = Fix <$> (PreprocIf c <$> f t <*> recurse f e)
recurse f (Fix (PreprocIfdef c t e))  = Fix <$> (PreprocIfdef c <$> f t <*> recurse f e)
recurse f (Fix (PreprocIfndef c t e)) = Fix <$> (PreprocIfndef c <$> f t <*> recurse f e)
recurse f (Fix (PreprocIfndef c t e)) = Fix <$> (PreprocIfndef c <$> f t <*> recurse f e)
recurse f (Fix (PreprocElif c t e))   = Fix <$> (PreprocElif c <$> f t <*> recurse f e)
recurse f (Fix (PreprocElse []))      = return $ Fix $ PreprocElse []
recurse f (Fix (PreprocElse e))       = Fix <$> (PreprocElse <$> f e)
recurse _ ns                          = fail $ "TreeParser.recurse: " <> show ns


parseError :: ([TextNode], [String]) -> TreeParser a
parseError ([], options)  = fail $ "end of file; expected one of " <> show options
parseError (n:_, [])      = fail $ show n <> "; expected end of file"
parseError (n:_, options) = fail $ show n <> "; expected one of " <> show options
}

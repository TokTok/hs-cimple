{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Language.Cimple.TreeParser (parseTranslationUnit) where

import           Data.Fix                      (Fix (..))
import           Data.Maybe                    (maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Cimple.Ast           (CommentStyle (..), Node,
                                                NodeF (..))
import           Language.Cimple.CommentParser (parseComment)
import           Language.Cimple.DescribeAst   (describeNode, sloc)
import           Language.Cimple.Lexer         (Lexeme (..))
import           Language.Cimple.ParseResult   (ParseResult)
import           Language.Cimple.Tokens        (LexemeClass (..))
}

%name parseTranslationUnit TranslationUnit
%name parseDecls Decls
%name parseMemberDecls MemberDecls

%error {parseError}
%errorhandlertype explist
%monad {ParseResult}
%tokentype {NonTerm}
%token
    ifndefDefine	{ Fix (PreprocIfndef _ (isDefine -> True) (Fix (PreprocElse []))) }
    ifdefDefine		{ Fix (PreprocIfdef _ (isDefine -> True) (Fix (PreprocElse []))) }
    ifDefine		{ Fix (PreprocIf _ (isDefine -> True) (Fix (PreprocElse []))) }

    docComment		{ Fix (Comment Doxygen _ _ _) }

    -- Preprocessor
    localIncludeBlock	{ (isIncludeBlock LitString -> True) }
    sysIncludeBlock	{ (isIncludeBlock LitSysInclude -> True) }
    localInclude	{ Fix (PreprocInclude (L _ LitString _)) }
    sysInclude		{ Fix (PreprocInclude (L _ LitSysInclude _)) }

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
    commentSectionStart	{ Fix (Comment Section _ _ _) }
    commentSectionEnd	{ Fix (CommentSectionEnd{}) }
    comment		{ Fix (Comment{}) }
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
    exprStmt		{ Fix (ExprStmt{}) }
    -- Variable declarations
    vLA			{ Fix (VLA{}) }
    varDeclStmt		{ Fix (VarDecl{}) }
    varDecl		{ Fix (VarDecl{}) }
    declSpecArray	{ Fix (DeclSpecArray{}) }
    -- Expressions
    initialiserList	{ Fix (InitialiserList{}) }
    unaryExpr		{ Fix (UnaryExpr{}) }
    binaryExpr		{ Fix (BinaryExpr{}) }
    ternaryExpr		{ Fix (TernaryExpr{}) }
    assignExpr		{ Fix (AssignExpr{}) }
    parenExpr		{ Fix (ParenExpr{}) }
    castExpr		{ Fix (CastExpr{}) }
    compoundLiteral	{ Fix (CompoundLiteral{}) }
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
    aggregateDecl	{ Fix (AggregateDecl{}) }
    typedefFunction	{ Fix (TypedefFunction{}) }
    typedefStruct	{ Fix (Typedef (Fix Struct{}) _ _) }
    typedef		{ Fix (Typedef{}) }
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
    ellipsis		{ Fix (Ellipsis) }
    nonNull		{ Fix (NonNull{}) }
    attrPrintf		{ Fix (AttrPrintf{}) }
    -- Constants
    constDecl		{ Fix (ConstDecl{}) }
    constDefn		{ Fix (ConstDefn{}) }

%%

TranslationUnit :: { [NonTerm] }
TranslationUnit
:	licenseDecl Decls					{ $1 : $2 }
|	            Decls					{ $1 }

Decls :: { [NonTerm] }
Decls
:	NonEmptyList(Decl)					{ $1 }

Decl :: { NonTerm }
Decl
:	comment							{ $1 }
|	commentSectionStart Decls commentSectionEnd		{ Fix $ CommentSection $1 $2 $3 }
|	CommentableDecl						{ $1 }
|	docComment CommentableDecl				{% fmap (\c -> Fix $ Commented c $2) $ parseDocComment $1 }

CommentableDecl :: { NonTerm }
CommentableDecl
:	functionDecl						{ $1 }
|	functionDefn						{ $1 }
|	nonNull							{ $1 }
|	attrPrintf						{ $1 }
|	aggregateDecl						{% processAggregate $1 }
|	struct							{% processAggregate $1 }
|	union							{% processAggregate $1 }
|	typedefStruct						{% processAggregate $1 }
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
|	ifndefDefine						{ $1 }
|	ifdefDefine						{ $1 }
|	ifDefine						{ $1 }
|	localInclude						{ $1 }
|	localIncludeBlock					{ $1 }
|	sysInclude						{ $1 }
|	sysIncludeBlock						{ $1 }

MemberDecls :: { [NonTerm] }
MemberDecls
:	NonEmptyList(MemberDecl)				{ $1 }

MemberDecl :: { NonTerm }
MemberDecl
:	comment							{ $1 }
|	memberDecl						{ $1 }
|	docComment memberDecl					{% fmap (\c -> Fix $ Commented c $2) $ parseDocComment $1 }
|	preprocIfdef						{% recurse parseMemberDecls $1 }
|	preprocIfndef						{% recurse parseMemberDecls $1 }
|	preprocIf						{% recurse parseMemberDecls $1 }

List(x)
:								{ [] }
|	NonEmptyList(x)						{ $1 }

NonEmptyList(x)
:	NonEmptyList_(x)					{ reverse $1 }

NonEmptyList_(x)
:	x							{ [$1] }
|	NonEmptyList_(x) x					{ $2 : $1 }

{
type TextLexeme = Lexeme Text
type NonTerm = Node TextLexeme

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs


isDefine :: [NonTerm] -> Bool
isDefine (Fix PreprocUndef{}:d)     = isDefine d
isDefine [Fix PreprocDefine{}]      = True
isDefine [Fix PreprocDefineConst{}] = True
isDefine _                          = False

isIncludeBlock :: LexemeClass -> NonTerm -> Bool
isIncludeBlock style tk@(Fix PreprocIfndef{}) = isPreproc tk && hasInclude style tk
isIncludeBlock style tk@(Fix PreprocIfdef{})  = isPreproc tk && hasInclude style tk
isIncludeBlock style tk@(Fix PreprocIf{})     = isPreproc tk && hasInclude style tk
isIncludeBlock _ _                            = False

isPreproc :: NonTerm -> Bool
isPreproc (Fix PreprocInclude{})        = True
isPreproc (Fix PreprocUndef{})          = True
isPreproc (Fix PreprocDefine{})         = True
isPreproc (Fix PreprocDefineConst{})    = True
isPreproc (Fix (PreprocIf _ td ed))     = all isPreproc td && isPreproc ed
isPreproc (Fix (PreprocIfdef _ td ed))  = all isPreproc td && isPreproc ed
isPreproc (Fix (PreprocIfndef _ td ed)) = all isPreproc td && isPreproc ed
isPreproc (Fix (PreprocElse ed))        = all isPreproc ed
isPreproc _                             = False

hasInclude :: LexemeClass -> NonTerm -> Bool
hasInclude style (Fix (PreprocInclude (L _ c _))) = c == style
hasInclude style (Fix (PreprocIf _ td ed))        = any (hasInclude style) td || (hasInclude style) ed
hasInclude style (Fix (PreprocIfdef _ td ed))     = any (hasInclude style) td || (hasInclude style) ed
hasInclude style (Fix (PreprocIfndef _ td ed))    = any (hasInclude style) td || (hasInclude style) ed
hasInclude style (Fix (PreprocElse ed))           = any (hasInclude style) ed
hasInclude _ _                                    = False

-- This helper function unpacks a struct/union, processes its members,
-- and then packs it back up.
processAggregate :: NonTerm -> ParseResult NonTerm
processAggregate (Fix (Struct name members)) = do
    processedMembers <- parseMemberDecls members
    return $ Fix (Struct name processedMembers)
processAggregate (Fix (Union name members)) = do
    processedMembers <- parseMemberDecls members
    return $ Fix (Union name processedMembers)
processAggregate (Fix (AggregateDecl agg)) = do
    processedAgg <- processAggregate agg
    return $ Fix (AggregateDecl processedAgg)
processAggregate (Fix (Typedef agg name arrs)) = do
    processedAgg <- processAggregate agg
    return $ Fix (Typedef processedAgg name arrs)
processAggregate n = return n -- Return other types unchanged

recurse :: ([NonTerm] -> ParseResult [NonTerm]) -> NonTerm -> ParseResult NonTerm
recurse f (Fix (ExternC ds))          = Fix <$> (ExternC <$> f ds)
recurse f (Fix (PreprocIf     c t e)) = Fix <$> (PreprocIf     c <$> f t <*> recurse f e)
recurse f (Fix (PreprocIfdef  c t e)) = Fix <$> (PreprocIfdef  c <$> f t <*> recurse f e)
recurse f (Fix (PreprocIfndef c t e)) = Fix <$> (PreprocIfndef c <$> f t <*> recurse f e)
recurse f (Fix (PreprocElif   c t e)) = Fix <$> (PreprocElif   c <$> f t <*> recurse f e)
recurse f (Fix (PreprocElse      [])) = Fix <$> pure (PreprocElse [])
recurse f (Fix (PreprocElse       e)) = Fix <$> (PreprocElse     <$>                 f e)
recurse _ ns                          = fail $ "TreeParser.recurse: " <> show ns

parseDocComment :: NonTerm -> ParseResult NonTerm
parseDocComment (Fix (Comment Doxygen start body end)) =
    Fix . CommentInfo <$> parseComment (start : body ++ [end])
parseDocComment n = return n

failAt :: NonTerm -> String -> ParseResult a
failAt n msg =
    fail $ Text.unpack (sloc "" n) <> ": unexpected " <> describeNode n <> msg

parseError :: ([NonTerm], [String]) -> ParseResult a
parseError ([], options)  = fail $ " end of file; expected one of " <> show options
parseError (n:_, [])      = failAt n "; expected end of file"
parseError (n:_, options) = failAt n $ "; expected one of " <> show options
}

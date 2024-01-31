{
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Cimple.Parser
    ( parseExpr
    , parseStmt
    , parseTranslationUnit
    , source
    ) where

import qualified Data.ByteString             as BS
import           Data.FileEmbed              (embedFile)
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple.Ast         (AssignOp (..), BinaryOp (..),
                                              CommentStyle (..),
                                              LiteralType (..), Node,
                                              NodeF (..), Scope (..),
                                              UnaryOp (..))
import           Language.Cimple.DescribeAst (parseError)
import           Language.Cimple.Lexer       (Alex, AlexPosn (..), Lexeme (..),
                                              alexError, alexMonadScan)
import           Language.Cimple.Tokens      (LexemeClass (..))
}

-- Conflict between (static) FunctionDecl and (static) ConstDecl.
%expect 2

%name parseTranslationUnit TranslationUnit
%name parseExpr Expr
%name parseStmt Stmt

%error {parseError}
%errorhandlertype explist
%lexer {lexwrap} {L _ Eof _}
%monad {Alex}
%tokentype {Term}
%token
    ID_CONST			{ L _ IdConst			_ }
    ID_FUNC_TYPE		{ L _ IdFuncType		_ }
    ID_STD_TYPE			{ L _ IdStdType			_ }
    ID_SUE_TYPE			{ L _ IdSueType			_ }
    ID_VAR			{ L _ IdVar			_ }
    break			{ L _ KwBreak			_ }
    case			{ L _ KwCase			_ }
    const			{ L _ KwConst			_ }
    constant			{ L _ KwConstant		_ }
    continue			{ L _ KwContinue		_ }
    default			{ L _ KwDefault			_ }
    do				{ L _ KwDo			_ }
    else			{ L _ KwElse			_ }
    enum			{ L _ KwEnum			_ }
    extern			{ L _ KwExtern			_ }
    for				{ L _ KwFor			_ }
    GNU_PRINTF			{ L _ KwGnuPrintf		_ }
    goto			{ L _ KwGoto			_ }
    if				{ L _ KwIf			_ }
    non_null			{ L _ KwNonNull			_ }
    nullable			{ L _ KwNullable		_ }
    return			{ L _ KwReturn			_ }
    sizeof			{ L _ KwSizeof			_ }
    static			{ L _ KwStatic			_ }
    static_assert		{ L _ KwStaticAssert		_ }
    struct			{ L _ KwStruct			_ }
    switch			{ L _ KwSwitch			_ }
    typedef			{ L _ KwTypedef			_ }
    union			{ L _ KwUnion			_ }
    VLA				{ L _ KwVla			_ }
    void			{ L _ KwVoid			_ }
    while			{ L _ KwWhile			_ }
    LIT_CHAR			{ L _ LitChar			_ }
    LIT_FALSE			{ L _ LitFalse			_ }
    LIT_TRUE			{ L _ LitTrue			_ }
    LIT_INTEGER			{ L _ LitInteger		_ }
    LIT_STRING			{ L _ LitString			_ }
    LIT_SYS_INCLUDE		{ L _ LitSysInclude		_ }
    '&'				{ L _ PctAmpersand		_ }
    '&&'			{ L _ PctAmpersandAmpersand	_ }
    '&='			{ L _ PctAmpersandEq		_ }
    '->'			{ L _ PctArrow			_ }
    '*'				{ L _ PctAsterisk		_ }
    '*='			{ L _ PctAsteriskEq		_ }
    '^'				{ L _ PctCaret			_ }
    '^='			{ L _ PctCaretEq		_ }
    ':'				{ L _ PctColon			_ }
    ','				{ L _ PctComma			_ }
    '!'				{ L _ PctEMark			_ }
    '!='			{ L _ PctEMarkEq		_ }
    '='				{ L _ PctEq			_ }
    '=='			{ L _ PctEqEq			_ }
    '>'				{ L _ PctGreater		_ }
    '>='			{ L _ PctGreaterEq		_ }
    '>>'			{ L _ PctGreaterGreater		_ }
    '>>='			{ L _ PctGreaterGreaterEq	_ }
    '{'				{ L _ PctLBrace			_ }
    '['				{ L _ PctLBrack			_ }
    '<'				{ L _ PctLess			_ }
    '<='			{ L _ PctLessEq			_ }
    '<<'			{ L _ PctLessLess		_ }
    '<<='			{ L _ PctLessLessEq		_ }
    '('				{ L _ PctLParen			_ }
    '-'				{ L _ PctMinus			_ }
    '-='			{ L _ PctMinusEq		_ }
    '--'			{ L _ PctMinusMinus		_ }
    '%'				{ L _ PctPercent		_ }
    '%='			{ L _ PctPercentEq		_ }
    '.'				{ L _ PctPeriod			_ }
    '...'			{ L _ PctEllipsis		_ }
    '|'				{ L _ PctPipe			_ }
    '|='			{ L _ PctPipeEq			_ }
    '||'			{ L _ PctPipePipe		_ }
    '+'				{ L _ PctPlus			_ }
    '+='			{ L _ PctPlusEq			_ }
    '++'			{ L _ PctPlusPlus		_ }
    '?'				{ L _ PctQMark			_ }
    '}'				{ L _ PctRBrace			_ }
    ']'				{ L _ PctRBrack			_ }
    ')'				{ L _ PctRParen			_ }
    ';'				{ L _ PctSemicolon		_ }
    '/'				{ L _ PctSlash			_ }
    '/='			{ L _ PctSlashEq		_ }
    '~'				{ L _ PctTilde			_ }
    'defined'			{ L _ PpDefined			_ }
    '#define'			{ L _ PpDefine			_ }
    '#elif'			{ L _ PpElif			_ }
    '#else'			{ L _ PpElse			_ }
    '#endif'			{ L _ PpEndif			_ }
    '#if'			{ L _ PpIf			_ }
    '#ifdef'			{ L _ PpIfdef			_ }
    '#ifndef'			{ L _ PpIfndef			_ }
    '#include'			{ L _ PpInclude			_ }
    '#undef'			{ L _ PpUndef			_ }
    '\n'			{ L _ PpNewline			_ }
    '/*'			{ L _ CmtStart			_ }
    '/*!'			{ L _ CmtStartCode		_ }
    '/**'			{ L _ CmtStartDoc		_ }
    '/** @{'			{ L _ CmtStartDocSection	_ }
    '/** @} */'			{ L _ CmtEndDocSection		_ }
    '/***'			{ L _ CmtStartBlock		_ }
    ' '				{ L _ CmtIndent			_ }
    '*/'			{ L _ CmtEnd			_ }
    'Copyright'			{ L _ CmtSpdxCopyright		_ }
    'License'			{ L _ CmtSpdxLicense		_ }
    CMT_ATTR			{ L _ CmtAttr			_ }
    CMT_CODE			{ L _ CmtCode			_ }
    CMT_COMMAND			{ L _ CmtCommand		_ }
    CMT_WORD			{ L _ CmtWord			_ }
    CMT_REF			{ L _ CmtRef			_ }
    IGN_START			{ L _ IgnStart			_ }
    IGN_BODY			{ L _ IgnBody			_ }
    IGN_END			{ L _ IgnEnd			_ }

%left ','
%right '=' '+=' '-=' '*=' '/=' '%=' '<<=' '>>=' '&=' '^=' '|='
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '!=' '=='
%left '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%right CAST ADDRESS NEG DEREF sizeof '!' '~' '++' '--'
%left '->' '.' '(' '['

%%

TranslationUnit :: { [NonTerm] }
TranslationUnit
:	ToplevelDecls						{ reverse $1 }
|	LicenseDecl ToplevelDecls				{ $1 : reverse $2 }

LicenseDecl :: { NonTerm }
LicenseDecl
:	'/*' 'License' CMT_WORD '\n' CopyrightDecls '*/'	{ Fix $ LicenseDecl $3 (reverse $5) }

CopyrightDecls :: { [NonTerm] }
CopyrightDecls
:	CopyrightDecl						{ [$1] }
|	CopyrightDecls CopyrightDecl				{ $2 : $1 }

CopyrightDecl :: { NonTerm }
CopyrightDecl
:	' ' 'Copyright' CopyrightDates CopyrightOwner '\n'	{ Fix $ CopyrightDecl (fst $3) (snd $3) $4 }

CopyrightDates :: { (Term, Maybe Term) }
CopyrightDates
:	LIT_INTEGER						{ ($1, Nothing) }
|	LIT_INTEGER '-' LIT_INTEGER				{ ($1, Just $3) }

CopyrightOwner :: { [Term] }
CopyrightOwner
:	CMT_WORD CommentWords					{ $1 : reverse $2 }

ToplevelDecls :: { [NonTerm] }
ToplevelDecls
:	ToplevelDecl						{ [$1] }
|	ToplevelDecls ToplevelDecl				{ $2 : $1 }

ToplevelDecl :: { NonTerm }
ToplevelDecl
:	AggregateDecl						{ $1 }
|	Comment							{ $1 }
|	ConstDecl						{ $1 }
|	EnumDecl						{ $1 }
|	ExternC							{ $1 }
|	FunctionDecl						{ $1 }
|	PreprocDefine						{ $1 }
|	PreprocIfdef(ToplevelDecls)				{ $1 }
|	PreprocIf(ToplevelDecls)				{ $1 }
|	PreprocInclude						{ $1 }
|	PreprocUndef						{ $1 }
|	ConstantDefn						{ $1 }
|	StaticAssert						{ $1 }
|	TypedefDecl						{ $1 }

ConstantDefn :: { NonTerm }
ConstantDefn
:	ConstantType ',' ID_CONST ',' ConstantValue ')' ';'	{ Fix $ PreprocDefineConst $3 $5 }

ConstantType :: { Term }
ConstantType
:	constant '(' ID_STD_TYPE				{ $3 }

ConstantValue :: { NonTerm }
ConstantValue
:	PreprocSafeExpr(ConstExpr)				{ $1 }

StaticAssert :: { NonTerm }
StaticAssert
:	static_assert '(' ConstExpr ',' LIT_STRING ')' ';'	{ Fix $ StaticAssert $3 $5 }

Comment :: { NonTerm }
Comment
:	'/*' CommentTokens '*/'					{ Fix $ Comment Regular $1 (reverse $2) $3 }
|	'/**' CommentTokens '*/'				{ Fix $ Comment Doxygen $1 (reverse $2) $3 }
|	'/** @{' CommentTokens '*/'				{ Fix $ Comment Section $1 (reverse $2) $3 }
|	'/***' CommentTokens '*/'				{ Fix $ Comment Block $1 (reverse $2) $3 }
|	'/** @} */'						{ Fix $ CommentSectionEnd $1 }
|	Ignore							{ $1 }

CommentTokens :: { [Term] }
CommentTokens
:	CommentToken						{ [$1] }
|	CommentTokens CommentToken				{ $2 : $1 }

CommentToken :: { Term }
CommentToken
:	CommentWord						{ $1 }
|	'\n'							{ $1 }
|	' '							{ $1 }

CommentWords :: { [Term] }
CommentWords
:								{ [] }
|	CommentWords CommentWord				{ $2 : $1 }

CommentWord :: { Term }
CommentWord
:	CMT_WORD						{ $1 }
|	CMT_COMMAND						{ $1 }
|	CMT_ATTR						{ $1 }
|	CMT_REF							{ $1 }
|	CMT_CODE						{ $1 }
|	LIT_INTEGER						{ $1 }
|	LIT_STRING						{ $1 }
|	'.'							{ $1 }
|	'...'							{ $1 }
|	'?'							{ $1 }
|	'!'							{ $1 }
|	','							{ $1 }
|	';'							{ $1 }
|	':'							{ $1 }
|	'('							{ $1 }
|	')'							{ $1 }
|	'<'							{ $1 }
|	'>'							{ $1 }
|	'/'							{ $1 }
|	'+'							{ $1 }
|	'-'							{ $1 }
|	'='							{ $1 }
|	'=='							{ $1 }
|	'!='							{ $1 }
|	'>='							{ $1 }

Ignore :: { NonTerm }
Ignore
:	IGN_START IgnoreBody IGN_END				{ Fix $ Comment Ignore $1 (reverse $2) $3 }

IgnoreBody :: { [Term] }
IgnoreBody
:	IGN_BODY						{ [$1] }
|	IgnoreBody IGN_BODY					{ $2 : $1 }

PreprocIfdef(decls)
:	'#ifdef' ID_CONST decls PreprocElse(decls) Endif	{ Fix $ PreprocIfdef $2 (reverse $3) $4 }
|	'#ifndef' ID_CONST decls PreprocElse(decls) Endif	{ Fix $ PreprocIfndef $2 (reverse $3) $4 }

PreprocIf(decls)
:	'#if' PreprocConstExpr '\n' decls PreprocElif(decls) Endif	{ Fix $ PreprocIf $2 (reverse $4) $5 }

PreprocElif(decls)
:	PreprocElse(decls)					{ $1 }
|	'#elif' PreprocConstExpr '\n' decls PreprocElif(decls)	{ Fix $ PreprocElif $2 (reverse $4) $5 }

PreprocElse(decls)
:								{ Fix $ PreprocElse [] }
|	'#else' decls						{ Fix $ PreprocElse (reverse $2) }

Endif :: { [Term] }
Endif
:	'#endif' Comment					{ [$1] }

PreprocInclude :: { NonTerm }
PreprocInclude
:	'#include' LIT_STRING					{ Fix $ PreprocInclude $2 }
|	'#include' LIT_SYS_INCLUDE				{ Fix $ PreprocInclude $2 }

PreprocDefine :: { NonTerm }
PreprocDefine
:	'#define' ID_CONST '\n'					{ Fix $ PreprocDefine $2 }
|	'#define' ID_CONST ConstantValue '\n'			{ Fix $ PreprocDefineConst $2 $3 }
|	'#define' ID_CONST MacroParamList MacroBody '\n'	{ Fix $ PreprocDefineMacro $2 $3 $4 }

PreprocUndef :: { NonTerm }
PreprocUndef
:	'#undef' ID_CONST					{ Fix $ PreprocUndef $2 }

PreprocConstExpr :: { NonTerm }
PreprocConstExpr
:	PureExpr(PreprocConstExpr)				{ $1 }
|	'defined' '(' ID_CONST ')'				{ Fix $ PreprocDefined $3 }

MacroParamList :: { [NonTerm] }
MacroParamList
:	'(' ')'							{ [] }
|	'(' MacroParams ')'					{ reverse $2 }
|	'(' MacroParams ',' '...' ')'				{ reverse $ Fix Ellipsis : $2 }

MacroParams :: { [NonTerm] }
MacroParams
:	MacroParam						{ [$1] }
|	MacroParams ',' MacroParam				{ $3 : $1 }

MacroParam :: { NonTerm }
MacroParam
:	ID_VAR							{ Fix $ MacroParam $1 }

MacroBody :: { NonTerm }
MacroBody
:	do CompoundStmt while '(' LIT_INTEGER ')'		{% macroBodyStmt $2 $5 }
|	FunctionCall						{ Fix $ MacroBodyFunCall $1 }

ExternC :: { NonTerm }
ExternC
:	'#ifdef' ID_CONST
	extern LIT_STRING '{'
	'#endif'
	ToplevelDecls
	'#ifdef' ID_CONST
	'}' Comment
	'#endif'						{% externC $2 $4 (reverse $7) $9 }

Stmts :: { [NonTerm] }
Stmts
:	Stmt							{ [$1] }
|	Stmts Stmt						{ $2 : $1 }

Stmt :: { NonTerm }
Stmt
:	PreprocIfdef(Stmts)					{ $1 }
|	PreprocIf(Stmts)					{ $1 }
|	PreprocDefine Stmts PreprocUndef			{ Fix $ PreprocScopedDefine $1 (reverse $2) $3 }
|	DeclStmt						{ $1 }
|	CompoundStmt						{ $1 }
|	IfStmt(ReturnStmt)					{ $1 }
|	IfStmt(CompoundStmt)					{ $1 }
|	ForStmt							{ $1 }
|	WhileStmt						{ $1 }
|	DoWhileStmt						{ $1 }
|	StaticAssert						{ $1 }
|	AssignExpr ';'						{ Fix $ ExprStmt $1 }
|	ExprStmt ';'						{ Fix $ ExprStmt $1 }
|	FunctionCall ';'					{ Fix $ ExprStmt $1 }
|	break ';'						{ Fix $ Break }
|	goto ID_CONST ';'					{ Fix $ Goto $2 }
|	ID_CONST ':' Stmt					{ Fix $ Label $1 $3 }
|	continue ';'						{ Fix $ Continue }
|	switch '(' Expr ')' '{' SwitchCases '}'			{ Fix $ SwitchStmt $3 (reverse $6) }
|	ReturnStmt						{ $1 }
|	Comment							{ $1 }

ReturnStmt :: { NonTerm }
ReturnStmt
:	return ';'						{ Fix $ Return Nothing }
|	return Expr ';'						{ Fix $ Return (Just $2) }

IfStmt(x)
:	if '(' Expr ')' x					{ Fix $ IfStmt $3 $5 Nothing }
|	if '(' Expr ')' x else x				{ Fix $ IfStmt $3 $5 (Just $7) }
|	if '(' Expr ')' x else IfStmt(x)			{ Fix $ IfStmt $3 $5 (Just $7) }

ForStmt :: { NonTerm }
ForStmt
:	for '(' ForInit Expr ';' ForNext ')' CompoundStmt	{ Fix $ ForStmt $3 $4 $6 $8 }

ForInit :: { NonTerm }
ForInit
:	AssignExpr ';'						{ Fix $ ExprStmt $1 }
|	VarDeclStmt						{ $1 }

ForNext :: { NonTerm }
ForNext
:	ExprStmt						{ $1 }
|	AssignExpr						{ $1 }

WhileStmt :: { NonTerm }
WhileStmt
:	while '(' Expr ')' CompoundStmt				{ Fix $ WhileStmt $3 $5 }

DoWhileStmt :: { NonTerm }
DoWhileStmt
:	do CompoundStmt while '(' Expr ')' ';'			{ Fix $ DoWhileStmt $2 $5 }

SwitchCases :: { [NonTerm] }
SwitchCases
:	SwitchCase						{ [$1] }
|	SwitchCases SwitchCase					{ $2 : $1 }

SwitchCase :: { NonTerm }
SwitchCase
:	case Expr ':' SwitchCaseBody				{ Fix $ Case $2 $4 }
|	default ':' SwitchCaseBody				{ Fix $ Default $3 }

SwitchCaseBody :: { NonTerm }
SwitchCaseBody
:	CompoundStmt						{ $1 }
|	SwitchCase						{ $1 }
|	break ';'						{ Fix $ Break }
|	return Expr ';'						{ Fix $ Return (Just $2) }

DeclStmt :: { NonTerm }
DeclStmt
:	VarDeclStmt						{ $1 }
|	VLA '(' QualType ',' ID_VAR ',' Expr ')' ';'		{ Fix $ VLA $3 $5 $7 }

VarDeclStmt :: { NonTerm }
VarDeclStmt
:	VarDecl '=' InitialiserExpr ';'				{ Fix $ VarDeclStmt $1 (Just $3) }
|	VarDecl ';'						{ Fix $ VarDeclStmt $1 Nothing }

VarDecl :: { NonTerm }
VarDecl
:	QualType ID_VAR DeclSpecArrays				{ Fix $ VarDecl $1 $2 $3 }
|	ID_FUNC_TYPE '*' ID_VAR DeclSpecArrays			{ Fix $ VarDecl (Fix $ TyPointer $ Fix $ TyFunc $1) $3 $4 }

DeclSpecArrays :: { [NonTerm] }
DeclSpecArrays
:								{ [] }
|	DeclSpecArrays DeclSpecArray				{ $2 : $1 }

DeclSpecArray :: { NonTerm }
DeclSpecArray
:	'[' ']'							{ Fix $ DeclSpecArray Nothing }
|	'[' Expr ']'						{ Fix $ DeclSpecArray (Just $2) }
|	'[' '/*!' Expr '*/' ']'					{ Fix $ DeclSpecArray (Just $3) }

InitialiserExpr :: { NonTerm }
InitialiserExpr
:	InitialiserList						{ Fix $ InitialiserList $1 }
|	Expr							{ $1 }

InitialiserList :: { [NonTerm] }
InitialiserList
:	'{' Initialisers '}'					{ reverse $2 }
|	'{' Initialisers ',' '}'				{ reverse $2 }

Initialisers :: { [NonTerm] }
Initialisers
:	Initialiser						{ [$1] }
|	Initialisers ',' Initialiser				{ $3 : $1 }

Initialiser :: { NonTerm }
Initialiser
:	Expr							{ $1 }
|	InitialiserList						{ Fix $ InitialiserList $1 }

CompoundStmt :: { NonTerm }
CompoundStmt
:	'{' Stmts '}'						{ Fix $ CompoundStmt (reverse $2) }

-- Expressions that are safe for use as macro body without () around it..
PreprocSafeExpr(x)
:	LiteralExpr						{ $1 }
|	'(' x ')'						{ Fix $ ParenExpr $2 }
|	'(' QualType ')' x %prec CAST				{ Fix $ CastExpr $2 $4 }
|	sizeof '(' Expr ')'					{ Fix $ SizeofExpr $3 }
|	sizeof '(' QualType ')'					{ Fix $ SizeofType $3 }

ConstExpr :: { NonTerm }
ConstExpr
:	PureExpr(ConstExpr)					{ $1 }

PureExpr(x)
:	PreprocSafeExpr(x)					{ $1 }
|	x '!=' x						{ Fix $ BinaryExpr $1 BopNe $3 }
|	x '==' x						{ Fix $ BinaryExpr $1 BopEq $3 }
|	x '||' x						{ Fix $ BinaryExpr $1 BopOr $3 }
|	x '^' x							{ Fix $ BinaryExpr $1 BopBitXor $3 }
|	x '|' x							{ Fix $ BinaryExpr $1 BopBitOr $3 }
|	x '&&' x						{ Fix $ BinaryExpr $1 BopAnd $3 }
|	x '&' x							{ Fix $ BinaryExpr $1 BopBitAnd $3 }
|	x '/' x							{ Fix $ BinaryExpr $1 BopDiv $3 }
|	x '*' x							{ Fix $ BinaryExpr $1 BopMul $3 }
|	x '%' x							{ Fix $ BinaryExpr $1 BopMod $3 }
|	x '+' x							{ Fix $ BinaryExpr $1 BopPlus $3 }
|	x '-' x							{ Fix $ BinaryExpr $1 BopMinus $3 }
|	x '<' x							{ Fix $ BinaryExpr $1 BopLt $3 }
|	x '<=' x						{ Fix $ BinaryExpr $1 BopLe $3 }
|	x '<<' x						{ Fix $ BinaryExpr $1 BopLsh $3 }
|	x '>' x							{ Fix $ BinaryExpr $1 BopGt $3 }
|	x '>=' x						{ Fix $ BinaryExpr $1 BopGe $3 }
|	x '>>' x						{ Fix $ BinaryExpr $1 BopRsh $3 }
|	x '?' x ':' x						{ Fix $ TernaryExpr $1 $3 $5 }
|	'!' x							{ Fix $ UnaryExpr UopNot $2 }
|	'~' x							{ Fix $ UnaryExpr UopNeg $2 }
|	'-' x %prec NEG						{ Fix $ UnaryExpr UopMinus $2 }
|	'&' x %prec ADDRESS					{ Fix $ UnaryExpr UopAddress $2 }

LiteralExpr :: { NonTerm }
LiteralExpr
:	StringLiteralExpr					{ $1 }
|	LIT_CHAR						{ Fix $ LiteralExpr Char $1 }
|	LIT_INTEGER						{ Fix $ LiteralExpr Int $1 }
|	LIT_FALSE						{ Fix $ LiteralExpr Bool $1 }
|	LIT_TRUE						{ Fix $ LiteralExpr Bool $1 }
|	ID_CONST						{ Fix $ LiteralExpr ConstId $1 }

StringLiteralExpr :: { NonTerm }
StringLiteralExpr
:	LIT_STRING						{ Fix $ LiteralExpr String $1 }
|	StringLiteralExpr LIT_STRING				{ $1 }

LhsExpr :: { NonTerm }
LhsExpr
:	ID_VAR							{ Fix $ VarExpr $1 }
|	'*' LhsExpr %prec DEREF					{ Fix $ UnaryExpr UopDeref $2 }
|	LhsExpr '.' ID_VAR					{ Fix $ MemberAccess $1 $3 }
|	LhsExpr '->' ID_VAR					{ Fix $ PointerAccess $1 $3 }
|	LhsExpr '[' Expr ']'					{ Fix $ ArrayAccess $1 $3 }

Expr :: { NonTerm }
Expr
:	LhsExpr							{ $1 }
|	ExprStmt						{ $1 }
|	FunctionCall						{ $1 }
|	CompoundLiteral						{ $1 }
|	PureExpr(Expr)						{ $1 }

-- Allow `(Type){0}` to set struct values to all-zero.
CompoundLiteral :: { NonTerm }
CompoundLiteral
:	'(' QualType ')' '{' ZeroInitExpr '}'			{ Fix $ CompoundLiteral $2 $5 }

ZeroInitExpr :: { NonTerm }
ZeroInitExpr
:	ID_VAR							{ Fix $ VarExpr $1 }
|	LIT_CHAR						{ Fix $ LiteralExpr Char $1 }
|	LIT_INTEGER						{ Fix $ LiteralExpr Int $1 }
|	LIT_FALSE						{ Fix $ LiteralExpr Bool $1 }
|	ID_CONST						{ Fix $ LiteralExpr ConstId $1 }
|	'{' ZeroInitExpr '}'					{ Fix $ InitialiserList [$2] }

AssignExpr :: { NonTerm }
AssignExpr
:	LhsExpr AssignOperator Expr				{ Fix $ AssignExpr $1 $2 $3 }

AssignOperator :: { AssignOp }
AssignOperator
:	'='							{ AopEq      }
|	'*='							{ AopMul     }
|	'/='							{ AopDiv     }
|	'+='							{ AopPlus    }
|	'-='							{ AopMinus   }
|	'&='							{ AopBitAnd  }
|	'|='							{ AopBitOr   }
|	'^='							{ AopBitXor  }
|	'%='							{ AopMod     }
|	'<<='							{ AopLsh     }
|	'>>='							{ AopRsh     }

ExprStmt :: { NonTerm }
ExprStmt
:	'++' Expr						{ Fix $ UnaryExpr UopIncr $2 }
|	'--' Expr						{ Fix $ UnaryExpr UopDecr $2 }

FunctionCall :: { NonTerm }
FunctionCall
:	Expr ArgList						{ Fix $ FunctionCall $1 $2 }

ArgList :: { [NonTerm] }
ArgList
:	'(' ')'							{ [] }
|	'(' Args ')'						{ reverse $2 }

Args :: { [NonTerm] }
Args
:	Arg							{ [$1] }
|	Args ',' Arg						{ $3 : $1 }

Arg :: { NonTerm }
Arg
:	Expr							{ $1 }
|	Comment Expr						{ Fix $ CommentExpr $1 $2 }

EnumDecl :: { NonTerm }
EnumDecl
:	enum ID_SUE_TYPE EnumeratorList ';'			{ Fix $ EnumConsts (Just $2) $3 }
|	enum             EnumeratorList ';'			{ Fix $ EnumConsts Nothing $2 }
|	typedef enum ID_SUE_TYPE EnumeratorList ID_SUE_TYPE ';'	{ Fix $ EnumDecl $3 $4 $5 }

EnumeratorList :: { [NonTerm] }
EnumeratorList
:	'{' Enumerators '}'					{ reverse $2 }
|	'{' Enumerators ',' '}'					{ reverse $2 }
|	'{' Enumerators Comment '}'				{ reverse $2 }  -- TODO(iphydf): Don't throw away the comment.
|	'{' Enumerators ',' Comment '}'				{ reverse $2 }  -- TODO(iphydf): Don't throw away the comment.

Enumerators :: { [NonTerm] }
Enumerators
:	Enumerator						{ [$1] }
|	Enumerators ',' Enumerator				{ $3 : $1 }

Enumerator :: { NonTerm }
Enumerator
:	EnumeratorName						{ Fix $ Enumerator $1 Nothing }
|	EnumeratorName '=' ConstExpr				{ Fix $ Enumerator $1 (Just $3) }
|	Comment Enumerator					{ Fix $ Commented $1 $2 }

EnumeratorName :: { Term }
EnumeratorName
:	ID_CONST						{ $1 }
|	ID_SUE_TYPE						{ $1 }

AggregateDecl :: { NonTerm }
AggregateDecl
:	AggregateType ';'					{ Fix $ AggregateDecl $1 }
|	typedef AggregateType ID_SUE_TYPE ';'			{ Fix $ Typedef $2 $3 }

AggregateType :: { NonTerm }
AggregateType
:	struct ID_SUE_TYPE '{' MemberDeclList '}'		{ Fix $ Struct $2 $4 }
|	union ID_SUE_TYPE '{' MemberDeclList '}'		{ Fix $ Union $2 $4 }

MemberDeclList :: { [NonTerm] }
MemberDeclList
:	MemberDecls						{ reverse $1 }

MemberDecls :: { [NonTerm] }
MemberDecls
:	MemberDecl						{ [$1] }
|	MemberDecls MemberDecl					{ $2 : $1 }

MemberDecl :: { NonTerm }
MemberDecl
:	VarDecl ';'						{ Fix $ MemberDecl $1 Nothing }
|	VarDecl ':' LIT_INTEGER ';'				{ Fix $ MemberDecl $1 (Just $3) }
|	PreprocIfdef(MemberDeclList)				{ $1 }
|	Comment							{ $1 }

TypedefDecl :: { NonTerm }
TypedefDecl
:	typedef QualType ID_SUE_TYPE ';'			{ Fix $ Typedef $2 $3 }
|	typedef FunctionPrototype(ID_FUNC_TYPE) ';'		{ Fix $ TypedefFunction $2 }

QualType :: { NonTerm }
QualType
:	LeafType						{                                        $1 }
|	LeafType '*'						{                              tyPointer $1 }
|	LeafType '*' '*'					{          tyPointer          (tyPointer $1) }
|	LeafType '*' '*' const					{ tyConst (tyPointer          (tyPointer $1)) }
|	LeafType '*' const					{                     tyConst (tyPointer $1) }
|	LeafType '*' const '*'					{          tyPointer (tyConst (tyPointer $1)) }
|	LeafType '*' const '*' const				{ tyConst (tyPointer (tyConst (tyPointer $1))) }
|	LeafType const						{                                         tyConst $1 }
|	LeafType const '*'					{                              tyPointer (tyConst $1) }
|	LeafType const '*' const				{                     tyConst (tyPointer (tyConst $1)) }
|	LeafType const '*' const '*'				{          tyPointer (tyConst (tyPointer (tyConst $1))) }
|	LeafType const '*' const '*' const			{ tyConst (tyPointer (tyConst (tyPointer (tyConst $1)))) }
|	const LeafType						{                                         tyConst $2 }
|	const LeafType '*'					{                              tyPointer (tyConst $2) }
|	const LeafType '*' const				{                     tyConst (tyPointer (tyConst $2)) }
|	const LeafType '*' const '*'				{          tyPointer (tyConst (tyPointer (tyConst $2))) }
|	const LeafType '*' const '*' const			{ tyConst (tyPointer (tyConst (tyPointer (tyConst $2)))) }

LeafType :: { NonTerm }
LeafType
:	struct ID_SUE_TYPE					{ Fix $ TyStruct $2 }
|	void							{ Fix $ TyStd $1 }
|	ID_STD_TYPE						{ Fix $ TyStd $1 }
|	ID_SUE_TYPE						{ Fix $ TyUserDefined $1 }

FunctionDecl :: { NonTerm }
FunctionDecl
:	FunctionDeclarator					{ $1 Global }
|	static FunctionDeclarator				{ $2 Static }
|	NonNull FunctionDeclarator				{ $1 $ $2 Global }
|	NonNull static FunctionDeclarator			{ $1 $ $3 Static }
|	NonNull Attrs FunctionDeclarator			{ $1 . $2 . $3 $ Global }

Attrs :: { NonTerm -> NonTerm }
Attrs
:	GNU_PRINTF '(' LIT_INTEGER ',' LIT_INTEGER ')'		{ Fix . AttrPrintf $3 $5 }

NonNull :: { NonTerm -> NonTerm }
NonNull
:	non_null '(' ')'					{ Fix . NonNull [] [] }
|	nullable '(' Ints ')'					{ Fix . NonNull [] $3 }
|	non_null '(' Ints ')' nullable '(' Ints ')'		{ Fix . NonNull $3 $7 }

Ints :: { [Term] }
Ints
:	IntList							{ reverse $1 }

IntList :: { [Term] }
IntList
:	LIT_INTEGER						{ [$1] }
|	IntList ',' LIT_INTEGER					{ $3 : $1 }

FunctionDeclarator :: { Scope -> NonTerm }
FunctionDeclarator
:	FunctionPrototype(ID_VAR) ';'				{ \s -> Fix $ FunctionDecl s $1 }
|	FunctionPrototype(ID_VAR) CompoundStmt			{ \s -> Fix $ FunctionDefn s $1 $2 }
|	CallbackDecl ';'					{ \s -> Fix $ FunctionDecl s $1 }

CallbackDecl :: { NonTerm }
CallbackDecl
:	ID_FUNC_TYPE ID_VAR					{ Fix $ CallbackDecl $1 $2 }

FunctionPrototype(id)
:	QualType id FunctionParamList				{ Fix $ FunctionPrototype $1 $2 $3 }
|	ID_FUNC_TYPE '*' id FunctionParamList			{ Fix $ FunctionPrototype (Fix $ TyPointer $ Fix $ TyFunc $1) $3 $4 }

FunctionParamList :: { [NonTerm] }
FunctionParamList
:	'(' ')'							{ [] }
|	'(' void ')'						{ [Fix $ TyStd $2] }
|	'(' FunctionParams ')'					{ reverse $2 }
|	'(' FunctionParams ',' '...' ')'			{ reverse $ Fix Ellipsis : $2 }

FunctionParams :: { [NonTerm] }
FunctionParams
:	FunctionParam						{ [$1] }
|	FunctionParams ',' FunctionParam			{ $3 : $1 }

FunctionParam :: { NonTerm }
FunctionParam
:	VarDecl							{ $1 }

ConstDecl :: { NonTerm }
ConstDecl
:	extern const LeafType ID_VAR ';'			{ Fix $ ConstDecl $3 $4 }
|	const LeafType ID_VAR '=' InitialiserExpr ';'		{ Fix $ ConstDefn Global $2 $3 $5 }
|	static const LeafType ID_VAR '=' InitialiserExpr ';'	{ Fix $ ConstDefn Static $3 $4 $6 }

{
type Term = Lexeme Text
type NonTerm = Node Term

tyPointer, tyConst :: NonTerm -> NonTerm
tyPointer = Fix . TyPointer
tyConst = Fix . TyConst

lexwrap :: (Lexeme Text -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

externC
    :: Term
    -> Term
    -> [NonTerm]
    -> Term
    -> Alex NonTerm
externC (L _ _ "__cplusplus") (L _ _ "\"C\"") decls (L _ _ "__cplusplus") =
    return $ Fix $ ExternC decls
externC _ lang _ _ =
    alexError $ show lang
        <> ": extern \"C\" declaration invalid (did you spell __cplusplus right?)"

macroBodyStmt
    :: NonTerm
    -> Term
    -> Alex NonTerm
macroBodyStmt decls (L _ _ "0") =
    return $ Fix $ MacroBodyStmt decls
macroBodyStmt _ cond =
    alexError $ show cond
        <> ": macro do-while body must end in 'while (0)'"

source :: Maybe BS.ByteString
#ifdef SOURCE
source = Just $(embedFile SOURCE)
#else
source = Nothing
#endif
}

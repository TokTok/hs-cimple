{
module Language.Cimple.Parser where

import           Language.Cimple.AST    (AssignOp (..), BinaryOp (..),
                                         CommentStyle (..), LiteralType (..),
                                         Node (..), Scope (..), UnaryOp (..))
import           Language.Cimple.Lexer  (Alex, AlexPosn, Lexeme (..), alexError,
                                         alexMonadScan)
import           Language.Cimple.Tokens (LexemeClass (..))
}

-- Conflict between (static) FunctionDecl and (static) ConstDecl.
%expect 2

%name parseCimple
%error {parseError}
%lexer {lexwrap} {L _ Eof _}
%monad {Alex}
%tokentype {Lexeme String}
%token
    ID_CONST			{ L _ IdConst			_ }
    ID_FUNC_TYPE		{ L _ IdFuncType		_ }
    ID_STD_TYPE			{ L _ IdStdType			_ }
    ID_SUE_TYPE			{ L _ IdSueType			_ }
    ID_TYVAR			{ L _ IdTyVar			_ }
    ID_VAR			{ L _ IdVar			_ }
    bitmask			{ L _ KwBitmask			_ }
    break			{ L _ KwBreak			_ }
    case			{ L _ KwCase			_ }
    class			{ L _ KwClass			_ }
    const			{ L _ KwConst			_ }
    continue			{ L _ KwContinue		_ }
    default			{ L _ KwDefault			_ }
    do				{ L _ KwDo			_ }
    else			{ L _ KwElse			_ }
    enum			{ L _ KwEnum			_ }
    'error'			{ L _ KwError			_ }
    event			{ L _ KwEvent			_ }
    extern			{ L _ KwExtern			_ }
    for				{ L _ KwFor			_ }
    goto			{ L _ KwGoto			_ }
    if				{ L _ KwIf			_ }
    namespace			{ L _ KwNamespace		_ }
    return			{ L _ KwReturn			_ }
    sizeof			{ L _ KwSizeof			_ }
    static			{ L _ KwStatic			_ }
    struct			{ L _ KwStruct			_ }
    switch			{ L _ KwSwitch			_ }
    this			{ L _ KwThis			_ }
    typedef			{ L _ KwTypedef			_ }
    union			{ L _ KwUnion			_ }
    VLA				{ L _ KwVla			_ }
    void			{ L _ KwVoid			_ }
    while			{ L _ KwWhile			_ }
    with			{ L _ KwWith			_ }
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
    '#error'			{ L _ PpError			_ }
    '#if'			{ L _ PpIf			_ }
    '#ifdef'			{ L _ PpIfdef			_ }
    '#ifndef'			{ L _ PpIfndef			_ }
    '#include'			{ L _ PpInclude			_ }
    '#undef'			{ L _ PpUndef			_ }
    '\n'			{ L _ PpNewline			_ }
    '/**/'			{ L _ CmtBlock			_ }
    '/*'			{ L _ CmtStart			_ }
    '/**'			{ L _ CmtStartDoc		_ }
    '/***'			{ L _ CmtStartBlock		_ }
    '*/'			{ L _ CmtEnd			_ }
    'Copyright'			{ L _ CmtSpdxCopyright		_ }
    'License'			{ L _ CmtSpdxLicense		_ }
    COMMENT_CODE		{ L _ CmtCode			_ }
    COMMENT_WORD		{ L _ CmtWord			_ }
    COMMENT_REF			{ L _ CmtRef			_ }

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

TranslationUnit :: { [StringNode] }
TranslationUnit
:	ToplevelDecls						{ reverse $1 }

ToplevelDecls :: { [StringNode] }
ToplevelDecls
:	ToplevelDecl						{ [$1] }
|	ToplevelDecls ToplevelDecl				{ $2 : $1 }

ToplevelDecl :: { StringNode }
ToplevelDecl
:	PreprocIfdef(TranslationUnit)				{ $1 }
|	PreprocIf(TranslationUnit)				{ $1 }
|	PreprocInclude						{ $1 }
|	PreprocDefine						{ $1 }
|	PreprocUndef						{ $1 }
|	PreprocError						{ $1 }
|	ExternC							{ $1 }
|	TypedefDecl						{ $1 }
|	AggregateDecl						{ $1 }
|	EnumDecl						{ $1 }
|	FunctionDecl						{ $1 }
|	ConstDecl						{ $1 }
|	Comment							{ $1 }
|	Namespace						{ $1 }
|	Event							{ $1 }
|	ErrorDecl						{ $1 }

Namespace :: { StringNode }
Namespace
:	NamespaceDeclarator					{ $1 Global }
|	static NamespaceDeclarator				{ $2 Static }

NamespaceDeclarator :: { Scope -> StringNode }
NamespaceDeclarator
:	class ID_SUE_TYPE TypeParams '{' TranslationUnit '}'	{ \s -> Class s $2 $3 $5 }
|	namespace IdVar '{' TranslationUnit '}'			{ \s -> Namespace s $2 $4 }

TypeParams :: { [StringNode] }
TypeParams
:								{ [] }
|	'<' ID_TYVAR '>'					{ [TyVar $2] }

Event :: { StringNode }
Event
:	event IdVar       '{' EventType '}'			{ Event $2 $4 }
|	event IdVar const '{' EventType '}'			{ Event $2 $5 }

EventType :: { StringNode }
EventType
:	Comment typedef void EventParams ';'			{ Commented $1 $4 }

EventParams :: { StringNode }
EventParams
:	FunctionParamList					{ EventParams $1 }

ErrorDecl :: { StringNode }
ErrorDecl
:	'error' for IdVar EnumeratorList			{ ErrorDecl $3 $4 }

Comment :: { StringNode }
Comment
:	'/*' CommentBody '*/'					{ Comment Regular (reverse $2) }
|	'/**' CommentBody '*/'					{ Comment Doxygen (reverse $2) }
|	'/***' CommentBody '*/'					{ Comment Block (reverse $2) }
|	'/**/'							{ CommentBlock $1 }

CommentBody :: { [StringNode] }
CommentBody
:	CommentWord						{ [$1] }
|	CommentBody CommentWord					{ $2 : $1 }

CommentWord :: { StringNode }
CommentWord
:	COMMENT_WORD						{ CommentWord $1 }
|	COMMENT_REF						{ CommentWord $1 }
|	COMMENT_CODE						{ CommentWord $1 }
|	LIT_INTEGER						{ CommentWord $1 }
|	LIT_STRING						{ CommentWord $1 }
|	'Copyright'						{ CommentWord $1 }
|	'License'						{ CommentWord $1 }
|	'.'							{ CommentWord $1 }
|	'?'							{ CommentWord $1 }
|	'!'							{ CommentWord $1 }
|	','							{ CommentWord $1 }
|	';'							{ CommentWord $1 }
|	':'							{ CommentWord $1 }
|	'('							{ CommentWord $1 }
|	')'							{ CommentWord $1 }
|	'<'							{ CommentWord $1 }
|	'>'							{ CommentWord $1 }
|	'/'							{ CommentWord $1 }
|	'+'							{ CommentWord $1 }
|	'-'							{ CommentWord $1 }
|	'='							{ CommentWord $1 }
|	'\n'							{ CommentWord $1 }

PreprocIfdef(decls)
:	'#ifdef' ID_CONST decls PreprocElse(decls) '#endif'	{ PreprocIfdef $2 $3 $4 }
|	'#ifndef' ID_CONST decls PreprocElse(decls) '#endif'	{ PreprocIfndef $2 $3 $4 }

PreprocIf(decls)
:	'#if' ConstExpr '\n' decls PreprocElse(decls) '#endif'	{ PreprocIf $2 $4 $5 }

PreprocElse(decls)
:								{ PreprocElse [] }
|	'#else' decls						{ PreprocElse $2 }
|	'#elif' ConstExpr '\n' decls PreprocElse(decls)		{ PreprocElif $2 $4 $5 }

PreprocError :: { StringNode }
PreprocError
:	'#error' LIT_STRING					{ PreprocError $2 }

PreprocInclude :: { StringNode }
PreprocInclude
:	'#include' LIT_STRING					{ PreprocInclude $2 }
|	'#include' LIT_SYS_INCLUDE				{ PreprocInclude $2 }

PreprocDefine :: { StringNode }
PreprocDefine
:	'#define' ID_CONST '\n'					{ PreprocDefine $2 }
|	'#define' ID_CONST ConstExpr '\n'			{ PreprocDefineConst $2 $3 }
|	'#define' ID_CONST MacroParamList MacroBody '\n'	{ PreprocDefineMacro $2 $3 $4 }

PreprocUndef :: { StringNode }
PreprocUndef
:	'#undef' ID_CONST					{ PreprocUndef $2 }

ConstExpr :: { StringNode }
ConstExpr
:	LiteralExpr						{ $1 }
|	'defined' '(' ID_CONST ')'				{ PreprocDefined $3 }
|	PureExpr(ConstExpr)					{ $1 }

MacroParamList :: { [StringNode] }
MacroParamList
:	'(' ')'							{ [] }
|	'(' MacroParams ')'					{ reverse $2 }
|	'(' MacroParams ',' '...' ')'				{ reverse $ Ellipsis : $2 }

MacroParams :: { [StringNode] }
MacroParams
:	MacroParam						{ [$1] }
|	MacroParams ',' MacroParam				{ $3 : $1 }

MacroParam :: { StringNode }
MacroParam
:	IdVar							{ MacroParam $1 }

MacroBody :: { StringNode }
MacroBody
:	do CompoundStmt while '(' LIT_INTEGER ')'		{% macroBodyStmt $2 $5 }
|	FunctionCall						{ MacroBodyFunCall $1 }

ExternC :: { StringNode }
ExternC
:	'#ifdef' ID_CONST
	extern LIT_STRING '{'
	'#endif'
	ToplevelDecls
	'#ifdef' ID_CONST
	'}'
	'#endif'						{% externC $2 $4 (reverse $7) $9 }

Stmts :: { [StringNode] }
Stmts
:	Stmt							{ [$1] }
|	Stmts Stmt						{ $2 : $1 }

Stmt :: { StringNode }
Stmt
:	PreprocIfdef(Stmts)					{ $1 }
|	PreprocIf(Stmts)					{ $1 }
|	PreprocDefine Stmts PreprocUndef			{ PreprocScopedDefine $1 $2 $3 }
|	LabelStmt						{ $1 }
|	DeclStmt						{ $1 }
|	CompoundStmt						{ CompoundStmt $1 }
|	IfStmt							{ $1 }
|	ForStmt							{ $1 }
|	WhileStmt						{ $1 }
|	DoWhileStmt						{ $1 }
|	AssignExpr ';'						{ $1 }
|	ExprStmt ';'						{ $1 }
|	FunctionCall ';'					{ $1 }
|	break ';'						{ Break }
|	goto ID_CONST ';'					{ Goto $2 }
|	continue ';'						{ Continue }
|	return ';'						{ Return Nothing }
|	return Expr ';'						{ Return (Just $2) }
|	switch '(' Expr ')' CompoundStmt			{ SwitchStmt $3 $5 }
|	Comment							{ $1 }

IfStmt :: { StringNode }
IfStmt
:	if '(' Expr ')' CompoundStmt				{ IfStmt $3 $5 Nothing }
|	if '(' Expr ')' CompoundStmt else IfStmt		{ IfStmt $3 $5 (Just $7) }
|	if '(' Expr ')' CompoundStmt else CompoundStmt		{ IfStmt $3 $5 (Just (CompoundStmt $7)) }

ForStmt :: { StringNode }
ForStmt
:	for '('
	ForInit
	Opt(Expr) ';'
	Opt(ForNext) ')'
	CompoundStmt						{ ForStmt $3 $4 $6 $8 }

ForInit :: { Maybe (StringNode) }
ForInit
:	';'							{ Nothing }
|	AssignExpr ';'						{ Just $1 }
|	SingleVarDecl						{ Just $1 }

ForNext :: { StringNode }
ForNext
:	ExprStmt						{ $1 }
|	AssignExpr						{ $1 }

Opt(x)
:								{ Nothing }
|	x							{ Just $1 }

WhileStmt :: { StringNode }
WhileStmt
:	while '(' Expr ')' CompoundStmt				{ WhileStmt $3 $5 }

DoWhileStmt :: { StringNode }
DoWhileStmt
:	do CompoundStmt while '(' Expr ')' ';'			{ DoWhileStmt $2 $5 }

LabelStmt :: { StringNode }
LabelStmt
:	case Expr ':' Stmt					{ Case $2 $4 }
|	default ':' Stmt					{ Default $3 }
|	ID_CONST ':' Stmt					{ Label $1 $3 }

DeclStmt :: { StringNode }
DeclStmt
:	VarDecl							{ $1 }
|	VLA '(' QualType ',' IdVar ',' Expr ')' ';'		{ VLA $3 $5 $7 }

SingleVarDecl :: { StringNode }
SingleVarDecl
:	QualType Declarator ';'					{ VarDecl $1 [$2] }

VarDecl :: { StringNode }
VarDecl
:	QualType Declarators ';'				{ VarDecl $1 (reverse $2) }

Declarators :: { [StringNode] }
Declarators
:	Declarator						{ [$1] }
|	Declarators ',' Declarator				{ $3 : $1 }

Declarator :: { StringNode }
Declarator
:	DeclSpec '=' InitialiserExpr				{ Declarator $1 (Just $3) }
|	DeclSpec						{ Declarator $1 Nothing }

InitialiserExpr :: { StringNode }
InitialiserExpr
:	InitialiserList						{ InitialiserList $1 }
|	Expr							{ $1 }

DeclSpec :: { StringNode }
DeclSpec
:	IdVar							{ DeclSpecVar $1 }
|	DeclSpec '[' ']'					{ DeclSpecArray $1 Nothing }
|	DeclSpec '[' Expr ']'					{ DeclSpecArray $1 (Just $3) }

IdVar :: { Lexeme String }
IdVar
:	ID_VAR							{ $1 }
|	default							{ $1 }
|	'error'							{ $1 }

InitialiserList :: { [StringNode] }
InitialiserList
:	'{' Initialisers '}'					{ reverse $2 }
|	'{' Initialisers ',' '}'				{ reverse $2 }

Initialisers :: { [StringNode] }
Initialisers
:	Initialiser						{ [$1] }
|	Initialisers ',' Initialiser				{ $3 : $1 }

Initialiser :: { StringNode }
Initialiser
:	Expr							{ $1 }
|	InitialiserList						{ InitialiserList $1 }

CompoundStmt :: { [StringNode] }
CompoundStmt
:	'{' Stmts '}'						{ reverse $2 }

PureExpr(x)
:	x '!=' x						{ BinaryExpr $1 BopNe $3 }
|	x '==' x						{ BinaryExpr $1 BopEq $3 }
|	x '||' x						{ BinaryExpr $1 BopOr $3 }
|	x '^' x							{ BinaryExpr $1 BopBitXor $3 }
|	x '|' x							{ BinaryExpr $1 BopBitOr $3 }
|	x '&&' x						{ BinaryExpr $1 BopAnd $3 }
|	x '&' x							{ BinaryExpr $1 BopBitAnd $3 }
|	x '/' x							{ BinaryExpr $1 BopDiv $3 }
|	x '*' x							{ BinaryExpr $1 BopMul $3 }
|	x '%' x							{ BinaryExpr $1 BopMod $3 }
|	x '+' x							{ BinaryExpr $1 BopPlus $3 }
|	x '-' x							{ BinaryExpr $1 BopMinus $3 }
|	x '<' x							{ BinaryExpr $1 BopLt $3 }
|	x '<=' x						{ BinaryExpr $1 BopLe $3 }
|	x '<<' x						{ BinaryExpr $1 BopLsh $3 }
|	x '>' x							{ BinaryExpr $1 BopGt $3 }
|	x '>=' x						{ BinaryExpr $1 BopGe $3 }
|	x '>>' x						{ BinaryExpr $1 BopRsh $3 }
|	x '?' x ':' x						{ TernaryExpr $1 $3 $5 }
|	'(' x ')'						{ ParenExpr $2 }
|	'!' x							{ UnaryExpr UopNot $2 }
|	'~' x							{ UnaryExpr UopNeg $2 }
|	'-' x %prec NEG						{ UnaryExpr UopMinus $2 }
|	'&' x %prec ADDRESS					{ UnaryExpr UopAddress $2 }
|	'(' QualType ')' x %prec CAST				{ CastExpr $2 $4 }
|	sizeof '(' x ')'					{ SizeofExpr $3 }
|	sizeof '(' QualType ')'					{ SizeofType $3 }

LiteralExpr :: { StringNode }
LiteralExpr
:	LIT_CHAR						{ LiteralExpr Char $1 }
|	LIT_INTEGER						{ LiteralExpr Int $1 }
|	LIT_FALSE						{ LiteralExpr Bool $1 }
|	LIT_TRUE						{ LiteralExpr Bool $1 }
|	LIT_STRING						{ LiteralExpr String $1 }
|	ID_CONST						{ LiteralExpr ConstId $1 }

Expr :: { StringNode }
Expr
:	LhsExpr							{ $1 }
|	ExprStmt						{ $1 }
|	LiteralExpr						{ $1 }
|	FunctionCall						{ $1 }
|	PureExpr(Expr)						{ $1 }

AssignExpr :: { StringNode }
AssignExpr
:	LhsExpr AssignOperator Expr				{ AssignExpr $1 $2 $3 }

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

ExprStmt :: { StringNode }
ExprStmt
:	'++' Expr						{ UnaryExpr UopIncr $2 }
|	'--' Expr						{ UnaryExpr UopDecr $2 }

LhsExpr :: { StringNode }
LhsExpr
:	IdVar							{ VarExpr $1 }
|	'*' LhsExpr %prec DEREF					{ UnaryExpr UopDeref $2 }
|	LhsExpr '.' IdVar					{ MemberAccess $1 $3 }
|	LhsExpr '->' IdVar					{ PointerAccess $1 $3 }
|	LhsExpr '[' Expr ']'					{ ArrayAccess $1 $3 }

FunctionCall :: { StringNode }
FunctionCall
:	Expr ArgList						{ FunctionCall $1 $2 }

ArgList :: { [StringNode] }
ArgList
:	'(' ')'							{ [] }
|	'(' Args ')'						{ reverse $2 }

Args :: { [StringNode] }
Args
:	Arg							{ [$1] }
|	Args ',' Arg						{ $3 : $1 }

Arg :: { StringNode }
Arg
:	Expr							{ $1 }
|	Comment Expr						{ CommentExpr $1 $2 }

EnumDecl :: { StringNode }
EnumDecl
:	enum class ID_SUE_TYPE EnumeratorList			{ EnumClass $3 $4 }
|	enum       ID_SUE_TYPE EnumeratorList			{ EnumConsts (Just $2) $3 }
|	enum                   EnumeratorList ';'		{ EnumConsts Nothing $2 }
|	typedef enum ID_SUE_TYPE EnumeratorList ID_SUE_TYPE ';'	{ EnumDecl $3 $4 $5 }
|	bitmask ID_SUE_TYPE EnumeratorList			{ EnumDecl $2 $3 $2 }

EnumeratorList :: { [StringNode] }
EnumeratorList
:	'{' Enumerators '}'					{ reverse $2 }

Enumerators :: { [StringNode] }
Enumerators
:	Enumerator						{ [$1] }
|	Enumerators Enumerator					{ $2 : $1 }

Enumerator :: { StringNode }
Enumerator
:	EnumeratorName ','					{ Enumerator $1 Nothing }
|	EnumeratorName '=' ConstExpr ','			{ Enumerator $1 (Just $3) }
|	namespace ID_CONST '{' Enumerators '}'			{ Namespace Global $2 $4 }
|	Comment							{ $1 }

EnumeratorName :: { Lexeme String }
EnumeratorName
:	ID_CONST						{ $1 }
|	ID_SUE_TYPE						{ $1 }

AggregateDecl :: { StringNode }
AggregateDecl
:	AggregateType ';'					{ $1 }
|	class ID_SUE_TYPE TypeParams ';'			{ ClassForward $2 $3 }
|	typedef AggregateType ID_SUE_TYPE ';'			{ Typedef $2 $3 }

AggregateType :: { StringNode }
AggregateType
:	struct ID_SUE_TYPE '{' MemberDeclList '}'		{ Struct $2 $4 }
|	struct this '{' MemberDeclList '}'			{ Struct $2 $4 }
|	union ID_SUE_TYPE '{' MemberDeclList '}'		{ Union $2 $4 }

MemberDeclList :: { [StringNode] }
MemberDeclList
:	MemberDecls						{ reverse $1 }

MemberDecls :: { [StringNode] }
MemberDecls
:	MemberDecl						{ [$1] }
|	MemberDecls MemberDecl					{ $2 : $1 }

MemberDecl :: { StringNode }
MemberDecl
:	QualType DeclSpec ';'					{ MemberDecl $1 $2 Nothing }
|	QualType DeclSpec ':' LIT_INTEGER ';'			{ MemberDecl $1 $2 (Just $4) }
|	namespace IdVar '{' MemberDeclList '}'			{ Namespace Global $2 $4 }
|	PreprocIfdef(MemberDeclList)				{ $1 }
|	Comment							{ $1 }

TypedefDecl :: { StringNode }
TypedefDecl
:	typedef QualType ID_SUE_TYPE ';'			{ Typedef $2 $3 }
|	typedef FunctionPrototype(ID_FUNC_TYPE) ';'		{ TypedefFunction $2 }

QualType :: { StringNode }
QualType
:	LeafType						{                               $1 }
|	LeafType '*'						{                     TyPointer $1 }
|	LeafType '*' '*'					{ TyPointer          (TyPointer $1) }
|	LeafType '*' const					{            TyConst (TyPointer $1) }
|	LeafType '*' const '*'					{ TyPointer (TyConst (TyPointer $1)) }
|	LeafType const						{                                TyConst $1 }
|	LeafType const '*'					{                     TyPointer (TyConst $1) }
|	LeafType const '*' const				{            TyConst (TyPointer (TyConst $1)) }
|	LeafType const '*' const '*'				{ TyPointer (TyConst (TyPointer (TyConst $1))) }
|	const LeafType						{                                TyConst $2 }
|	const LeafType '*'					{                     TyPointer (TyConst $2) }
|	const LeafType '*' const				{            TyConst (TyPointer (TyConst $2)) }
|	const LeafType '*' const '*'				{ TyPointer (TyConst (TyPointer (TyConst $2))) }

LeafType :: { StringNode }
LeafType
:	struct ID_SUE_TYPE					{ TyStruct $2 }
|	void							{ TyStd $1 }
|	this							{ TyStd $1 }
|	ID_FUNC_TYPE						{ TyFunc $1 }
|	ID_STD_TYPE						{ TyStd $1 }
|	ID_SUE_TYPE						{ TyUserDefined $1 }
|	ID_TYVAR						{ TyVar $1 }

FunctionDecl :: { StringNode }
FunctionDecl
:	FunctionDeclarator					{ $1 Global }
|	static FunctionDeclarator				{ $2 Static }

FunctionDeclarator :: { Scope -> StringNode }
FunctionDeclarator
:	FunctionPrototype(IdVar) WithError			{ \s -> FunctionDecl s $1 $2 }
|	FunctionPrototype(IdVar) CompoundStmt			{ \s -> FunctionDefn s $1 $2 }
|	QualType DeclSpec '{' AccessorList '}'			{ \s -> Property $1 $2 $4 }

AccessorList :: { [StringNode] }
AccessorList
:	Accessors						{ reverse $1 }

Accessors :: { [StringNode] }
Accessors
:	Accessor						{ [$1] }
|	Accessors Accessor					{ $2 : $1 }

Accessor :: { StringNode }
Accessor
:	IdVar FunctionParamList WithError			{ Accessor $1 $2 $3 }
|	Comment							{ $1 }

WithError :: { Maybe StringNode }
WithError
:	';'							{ Nothing }
|	with 'error' EnumeratorList				{ Just (ErrorList $3) }
|	with 'error' for IdVar ';'				{ Just (ErrorFor $4) }

FunctionPrototype(id)
:	QualType id FunctionParamList				{ FunctionPrototype $1 $2 $3 }
|	QualType id FunctionParamList const			{ FunctionPrototype $1 $2 $3 }

FunctionParamList :: { [StringNode] }
FunctionParamList
:	'(' ')'							{ [] }
|	'(' void ')'						{ [TyStd $2] }
|	'(' FunctionParams ')'					{ reverse $2 }
|	'(' FunctionParams ',' '...' ')'			{ reverse $ Ellipsis : $2 }

FunctionParams :: { [StringNode] }
FunctionParams
:	FunctionParam						{ [$1] }
|	FunctionParams ',' FunctionParam			{ $3 : $1 }

FunctionParam :: { StringNode }
FunctionParam
:	QualType DeclSpec					{ FunctionParam $1 $2 }

ConstDecl :: { StringNode }
ConstDecl
:	extern const LeafType ID_VAR ';'			{ ConstDecl $3 $4 }
|	const LeafType ID_VAR '=' InitialiserExpr ';'		{ ConstDefn Global $2 $3 $5 }
|	static const LeafType ID_VAR '=' InitialiserExpr ';'	{ ConstDefn Static $3 $4 $6 }

{
type StringNode = Node (Lexeme String)

parseError :: Show text => Lexeme text -> Alex a
parseError token = alexError $ "Parse error near token: " <> show token

lexwrap :: (Lexeme String -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

externC
    :: Lexeme String
    -> Lexeme String
    -> [StringNode]
    -> Lexeme String
    -> Alex StringNode
externC (L _ _ "__cplusplus") (L _ _ "\"C\"") decls (L _ _ "__cplusplus") =
    return $ ExternC decls
externC _ lang _ _ =
    alexError $ show lang
        <> ": extern \"C\" declaration invalid (did you spell __cplusplus right?)"

macroBodyStmt
    :: [StringNode]
    -> Lexeme String
    -> Alex StringNode
macroBodyStmt decls (L _ _ "0") =
    return $ MacroBodyStmt decls
macroBodyStmt _ cond =
    alexError $ show cond
        <> ": macro do-while body must end in 'while (0)'"
}

{
{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.CommentParser
    ( parseComment
    ) where

import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Language.Cimple.Ast         (AssignOp (..), BinaryOp (..),
                                              Comment, CommentF (..))
import           Language.Cimple.DescribeAst (describeLexeme, sloc)
import           Language.Cimple.Lexer       (Lexeme (..))
import           Language.Cimple.ParseResult (ParseResult)
import           Language.Cimple.Tokens      (LexemeClass (..))
}

%name parseComment Comment

%expect 1

%error {parseError}
%errorhandlertype explist
%monad {ParseResult}
%tokentype {Term}
%token
    '@attention'		{ L _ CmtCommand "@attention"	}
    '@brief'			{ L _ CmtCommand "@brief"	}
    '@deprecated'		{ L _ CmtCommand "@deprecated"	}
    '@implements'		{ L _ CmtCommand "@implements"	}
    '@param'			{ L _ CmtCommand "@param"	}
    '@ref'			{ L _ CmtCommand "@ref"		}
    '@p'			{ L _ CmtCommand "@p"		}
    '@return'			{ L _ CmtCommand "@return"	}
    '@retval'			{ L _ CmtCommand "@retval"	}
    '@see'			{ L _ CmtCommand "@see"		}

    ' '				{ L _ CmtIndent		" "	}
    'INDENT1'			{ L _ CmtIndent		"   "	}
    'INDENT2'			{ L _ CmtIndent		"    " 	}
    'INDENT3'			{ L _ CmtIndent		"     "	}

    '('				{ L _ PctLParen			_ }
    ')'				{ L _ PctRParen			_ }
    ','				{ L _ PctComma			_ }
    ':'				{ L _ PctColon			_ }
    '/'				{ L _ PctSlash			_ }
    '='				{ L _ PctEq			_ }
    '=='			{ L _ PctEqEq			_ }
    '!='			{ L _ PctEMarkEq		_ }
    '>='			{ L _ PctGreaterEq		_ }
    ';'				{ L _ PctSemicolon		_ }
    '.'				{ L _ PctPeriod			_ }
    '...'			{ L _ PctEllipsis		_ }
    '?'				{ L _ PctQMark			_ }
    '!'				{ L _ PctEMark			_ }
    '-'				{ L _ PctMinus			_ }
    '+'				{ L _ PctPlus			_ }
    '\n'			{ L _ PpNewline			_ }
    '/**'			{ L _ CmtStartDoc		_ }
    '*/'			{ L _ CmtEnd			_ }
    LIT_INTEGER			{ L _ LitInteger		_ }
    LIT_STRING			{ L _ LitString			_ }
    CMT_ATTR			{ L _ CmtAttr			_ }
    CMT_CODE			{ L _ CmtCode			_ }
    CMT_WORD			{ L _ CmtWord			_ }
    CMT_REF			{ L _ CmtRef			_ }

%right '='
%left '.' '?' ',' ';' '!'
%left '!=' '=='
%left '>='
%left '-' '+'
%left '/'
%left '(' ')'

%%

Comment :: { NonTerm }
Comment
:	'/**' '\n' Blocks '*/'					{ Fix $ DocComment $3 }
|	'/**' WordsWithoutNewlines '*/'				{ Fix $ DocComment [Fix $ DocLine $2] }
|	'/**' Command(WordsWithoutNewlines) '*/'		{ Fix $ DocComment [$2] }
|	'/**' Command(WordsWithoutNewlines) '\n' Blocks '*/'	{ Fix $ DocComment ($2 : $4) }

Blocks :: { [NonTerm] }
Blocks
:	BlockList						{ reverse $1 }

BlockList :: { [NonTerm] }
BlockList
:	Block							{ [$1] }
|	BlockList Block						{ $2 : $1 }

Block :: { NonTerm }
Block
:	'\n'							{ Fix DocNewline }
|	' ' Command(IndentedSentence)				{ $2 }
|	' ' Paragraph '\n'					{ Fix $ DocLine $2 }
|	' ' LIT_INTEGER '.' IndentedSentenceII			{ Fix $ DocLine $4 }
|	' ' '-' BulletListI					{ Fix $ DocBulletList (reverse $3) }

Paragraph :: { [NonTerm] }
Paragraph
:	Word(NonInt) Punctuation Words				{ Fix (DocSentence [$1] $2) : $3 }
|	Word(NonInt) Words					{ $1 : $2 }

Punctuation :: { Term }
Punctuation
:	'.'							{ $1 }
|	','							{ $1 }
|	';'							{ $1 }
|	'?'							{ $1 }

Words :: { [NonTerm] }
Words
:								{ [] }
|	SentenceList(WordList)					{ $1 }

Command(x)
:	'@attention' x						{ Fix $ DocAttention $2 }
|	'@brief' x						{ Fix $ DocBrief $2 }
|	'@param' CMT_WORD x					{ Fix $ DocParam Nothing $2 $3 }
|	'@param' CMT_ATTR CMT_WORD x				{ Fix $ DocParam (Just $2) $3 $4 }
|	'@retval' Atom x					{ Fix $ DocRetval $2 $3 }
|	'@return' x						{ Fix $ DocReturn $2 }
|	'@return' '\n' BulletListII				{ Fix $ DocReturn (Fix (DocLine []) : $3) }
|	'@see' CMT_WORD x					{ Fix $ DocSee $2 $3 }
|	'@deprecated' x						{ Fix $ DocDeprecated $2 }

BulletListI :: { [NonTerm] }
BulletListI
:	WordsWithoutNewlines '\n' BulletICont			{ [Fix $ DocBullet $1 $3] }

BulletICont :: { [NonTerm] }
BulletICont
:								{ [] }
|	' ' '-' BulletListI					{ $3 }
|	'INDENT1' BulletIContCont				{ $2 }

BulletIContCont :: { [NonTerm] }
BulletIContCont
:	WordsWithoutNewlines					{ $1 }
|	BulletIICont BulletListII				{ $1 : reverse $2 }

BulletListII :: { [NonTerm] }
BulletListII
:								{ [] }
|	BulletListII BulletII					{ $2 : $1 }

BulletII :: { NonTerm }
BulletII
:	'INDENT1' BulletIICont					{ $2 }

BulletIICont :: { NonTerm }
BulletIICont
:	'-' WordsWithoutNewlines '\n' BulletIIConts		{ Fix $ DocBullet ($2 ++ $4) [] }

BulletIIConts :: { [NonTerm] }
BulletIIConts
:								{ [] }
|	BulletIIConts BulletIIContCont				{ $1 ++ $2 }

BulletIIContCont :: { [NonTerm] }
BulletIIContCont
:	'INDENT3' WordsWithoutNewlines '\n'			{ $2 }

IndentedSentence :: { [NonTerm] }
IndentedSentence
:	WordsWithoutNewlines '\n'				{ [Fix $ DocLine $1] }
|	WordsWithoutNewlines '\n' 'INDENT1' IndentedSentence	{ Fix (DocLine $1) : $4 }

IndentedSentenceII :: { [NonTerm] }
IndentedSentenceII
:	WordsWithoutNewlines '\n'				{ [Fix $ DocLine $1] }
|	WordsWithoutNewlines '\n' 'INDENT2' IndentedSentenceII	{ Fix (DocLine $1) : $4 }

DocLine :: { [NonTerm] }
DocLine
:	CMT_WORD						{ [Fix (DocWord $1)] }
|	CMT_WORD '.'						{ [Fix (DocSentence [Fix (DocWord $1)] $2)] }
|	CMT_WORD WordsWithoutNewlines				{ Fix (DocWord $1) : $2 }
|	CMT_CODE WordsWithoutNewlines				{ Fix (DocWord $1) : $2 }

WordsWithoutNewlines :: { [NonTerm] }
WordsWithoutNewlines
:	SentenceList(WordList)					{ $1 }

WordList :: { [NonTerm] }
WordList
:	Word(Atom)						{ [$1] }
|	WordList Word(Atom)					{ $2 : $1 }

SentenceList(x)
:	x							{ reverse $1 }
|	SentenceList(x) ';'					{ [Fix (DocSentence $1 $2)] }
|	SentenceList(x) ','					{ [Fix (DocSentence $1 $2)] }
|	SentenceList(x) '.'					{ [Fix (DocSentence $1 $2)] }
|	SentenceList(x) '?'					{ [Fix (DocSentence $1 $2)] }
|	SentenceList(x) '!'					{ [Fix (DocSentence $1 $2)] }
|	SentenceList(x) ';' SentenceList(x)			{ Fix (DocSentence $1 $2) : $3 }
|	SentenceList(x) ',' SentenceList(x)			{ Fix (DocSentence $1 $2) : $3 }
|	SentenceList(x) '.' SentenceList(x)			{ Fix (DocSentence $1 $2) : $3 }
|	SentenceList(x) '?' SentenceList(x)			{ Fix (DocSentence $1 $2) : $3 }
|	SentenceList(x) '!' SentenceList(x)			{ Fix (DocSentence $1 $2) : $3 }

Word(x)
:	x							{ Fix $ DocWord $1 }
|	x ':'							{ Fix $ DocColon $1 }
|	Word(x) '=' Word(Atom)					{ Fix $ DocAssignOp AopEq $1 $3 }
|	Word(x) '+' Word(Atom)					{ Fix $ DocBinaryOp BopPlus $1 $3 }
|	Word(x) '-' Word(Atom)					{ Fix $ DocBinaryOp BopMinus $1 $3 }
|	Word(x) '/' Word(Atom)					{ Fix $ DocBinaryOp BopDiv $1 $3 }
|	Word(x) '!=' Word(Atom)					{ Fix $ DocBinaryOp BopNe $1 $3 }
|	Word(x) '>=' Word(Atom)					{ Fix $ DocBinaryOp BopGe $1 $3 }
|	Word(x) '==' Word(Atom)					{ Fix $ DocBinaryOp BopEq $1 $3 }
|	'@ref' Atom						{ Fix $ DocRef $2 }
|	'@p' Atom						{ Fix $ DocP $2 }
|	'(' Word(Atom)						{ Fix $ DocLParen $2 }
|	Word(x) ')'						{ Fix $ DocRParen $1 }

Atom :: { Term }
Atom
:	NonInt							{ $1 }
|	LIT_INTEGER						{ $1 }

NonInt :: { Term }
NonInt
:	CMT_WORD						{ $1 }
|	CMT_CODE						{ $1 }
|	LIT_STRING						{ $1 }
|	'...'							{ $1 }

{
type Term = Lexeme Text
type NonTerm = Comment Term

failAt :: Lexeme Text -> String -> ParseResult a
failAt n msg =
    fail $ Text.unpack (sloc "" n) <> ": unexpected " <> describeLexeme n <> msg

parseError :: ([Lexeme Text], [String]) -> ParseResult a
parseError ([], options)  = fail $ " end of comment; expected one of " <> show options
parseError (n:_, [])      = failAt n "; expected end of comment"
parseError (n:_, options) = failAt n $ "; expected one of " <> show options
}

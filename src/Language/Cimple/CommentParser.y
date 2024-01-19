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

%expect 0

%error {parseError}
%errorhandlertype explist
%monad {ParseResult}
%tokentype {Term}
%token
    '@attention'		{ L _ CmtCommand "@attention"	}
    '@brief'			{ L _ CmtCommand "@brief"	}
    '@deprecated'		{ L _ CmtCommand "@deprecated"	}
    '@extends'			{ L _ CmtCommand "@extends"	}
    '@implements'		{ L _ CmtCommand "@implements"	}
    '@note'			{ L _ CmtCommand "@note"	}
    '@param'			{ L _ CmtCommand "@param"	}
    '@private'			{ L _ CmtCommand "@private"	}
    '@ref'			{ L _ CmtCommand "@ref"		}
    '@p'			{ L _ CmtCommand "@p"		}
    '@return'			{ L _ CmtCommand "@return"	}
    '@retval'			{ L _ CmtCommand "@retval"	}
    '@see'			{ L _ CmtCommand "@see"		}
    '@code'			{ L _ CmtCode	 "@code"	}
    '@endcode'			{ L _ CmtCode	 "@endcode"	}

    ' '				{ L _ CmtIndent		" "	}
    'INDENT1'			{ L _ CmtIndent		"   "	}
    'INDENT2'			{ L _ CmtIndent		"    " 	}
    'INDENT3'			{ L _ CmtIndent		"     "	}
    'INDENT'			{ L _ CmtIndent			_ }

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
|	'/**' DocLine '*/'					{ Fix $ DocComment $2 }
|	'/**' Command(DocLine) '*/'				{ Fix $ DocComment [$2] }
|	'/**' Command(IndentedSentence) Blocks '*/'		{ Fix $ DocComment ($2 : $3) }

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
|	' ' Paragraph						{ Fix $ DocParagraph $2 }
|	' ' NumberedListItem					{ Fix $ DocList [$2] }
|	' ' BulletListItem					{ Fix $ DocList [$2] }

Paragraph :: { [NonTerm] }
Paragraph
:	Word(NonInt) Punctuation MaybeWords			{ Fix (DocSentence [$1] $2) : $3 }
|	Word(NonInt) MaybeWords					{ prepend $1 $2 }

Punctuation :: { Term }
Punctuation
:	'.'							{ $1 }
|	','							{ $1 }
|	';'							{ $1 }
|	'?'							{ $1 }

MaybeWords :: { [NonTerm] }
MaybeWords
:								{ [] }
|	IndentedSentence					{ $1 }

IndentedSentence :: { [NonTerm] }
IndentedSentence
:	DocLine '\n'						{ $1 }
|	DocLine '\n' 'INDENT1' IndentedSentence			{ $1 ++ $4 }

DocLine :: { [NonTerm] }
DocLine
:	Words							{ [Fix $ DocLine $1] }

Command(x)
:	'@attention' x						{ Fix $ DocAttention $2 }
|	'@brief' x						{ Fix $ DocBrief $2 }
|	'@param' CMT_WORD x					{ Fix $ DocParam Nothing $2 $3 }
|	'@param' CMT_ATTR CMT_WORD x				{ Fix $ DocParam (Just $2) $3 $4 }
|	'@retval' Atom x					{ Fix $ DocRetval $2 $3 }
|	'@return' x						{ Fix $ DocReturn $2 }
|	'@return' '\n' BulletListItemII				{ Fix $ DocReturn (Fix (DocLine []) : $3) }
|	'@see' CMT_WORD x					{ Fix $ DocSee $2 $3 }
|	'@deprecated' x						{ Fix $ DocDeprecated $2 }
|	'@implements' CMT_WORD					{ Fix $ DocImplements $2 }
|	'@extends' CMT_WORD					{ Fix $ DocExtends $2 }
|	'@private'						{ Fix DocPrivate }
|	Code							{ $1 }

Code :: { NonTerm }
Code
:	'@code' CodeWords '@endcode'				{ Fix $ DocCode $1 (reverse $2) $3 }

CodeWords :: { [NonTerm] }
CodeWords
:	CodeWord						{ [$1] }
|	CodeWords CodeWord					{ $2 : $1 }

CodeWord :: { NonTerm }
CodeWord
:	'\n'							{ Fix $ DocWord $1 }
|	' '							{ Fix $ DocWord $1 }
|	'INDENT1'						{ Fix $ DocWord $1 }
|	'INDENT2'						{ Fix $ DocWord $1 }
|	'INDENT3'						{ Fix $ DocWord $1 }
|	'INDENT'						{ Fix $ DocWord $1 }
|	CMT_CODE						{ Fix $ DocWord $1 }

BulletListItem :: { NonTerm }
BulletListItem
:	'-' Words '\n' BulletICont				{ Fix $ DocULItem $2 $4 }

BulletICont :: { [NonTerm] }
BulletICont
:								{ [] }
|	'INDENT1' DocLine '\n' BulletICont			{ $2 ++ $4 }
|	BulletListItemII					{ $1 }

BulletListItemII :: { [NonTerm] }
BulletListItemII
:	'INDENT1' '-' Words '\n' BulletIICont			{ Fix (DocULItem ($3 ++ snd $5) []) : fst $5 }

BulletIICont :: { ([NonTerm], [NonTerm]) }
BulletIICont
:								{ ([], []) }
|	BulletListItemII					{ ($1, []) }
|	'INDENT3' DocLine '\n' BulletIICont			{ ([], $2) <> $4 }

NumberedListItem :: { NonTerm }
NumberedListItem
:	LIT_INTEGER '.' IndentedSentenceII			{ Fix (DocOLItem $1 $3) }

IndentedSentenceII :: { [NonTerm] }
IndentedSentenceII
:	DocLine '\n'						{ $1 }
|	DocLine '\n' 'INDENT2' IndentedSentenceII		{ $1 ++ $4 }

Words :: { [NonTerm] }
Words
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

prepend :: NonTerm -> [NonTerm] -> [NonTerm]
prepend x [] = [x]
prepend x (Fix (DocLine xs):rest) = Fix (DocLine (x:xs)) : rest

failAt :: Lexeme Text -> String -> ParseResult a
failAt n msg =
    fail $ Text.unpack (sloc "" n) <> ": unexpected in comment: " <> describeLexeme n <> msg

parseError :: ([Lexeme Text], [String]) -> ParseResult a
parseError ([], options)  = fail $ " end of comment; expected one of " <> show options
parseError (n:_, [])      = failAt n "; expected end of comment"
parseError (n:_, options) = failAt n $ "; expected one of " <> show options
}

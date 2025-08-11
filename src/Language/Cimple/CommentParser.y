{
{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.CommentParser
    ( parseComment
    ) where

import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.List                   (dropWhile)

import           Language.Cimple.Ast         (Comment, CommentF (..))
import           Language.Cimple.DescribeAst (describeLexeme, sloc)
import           Language.Cimple.Lexer       (AlexPosn (..), Lexeme (..))
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
    '@attention'		{ L _ CmtCommand "@attention"		}
    '@brief'			{ L _ CmtCommand "@brief"		}
    '@deprecated'		{ L _ CmtCommand "@deprecated"		}
    '@file'			{ L _ CmtCommand "@file"		}
    '@extends'			{ L _ CmtCommand "@extends"		}
    '@implements'		{ L _ CmtCommand "@implements"		}
    '@note'			{ L _ CmtCommand "@note"		}
    '@param'			{ L _ CmtCommand "@param"		}
    '@private'			{ L _ CmtCommand "@private"		}
    '@ref'			{ L _ CmtCommand "@ref"			}
    '@p'			{ L _ CmtCommand "@p"			}
    '@return'			{ L _ CmtCommand "@return"		}
    '@retval'			{ L _ CmtCommand "@retval"		}
    '@section'			{ L _ CmtCommand "@section"		}
    '@subsection'		{ L _ CmtCommand "@subsection"		}
    '@see'			{ L _ CmtCommand "@see"			}
    '@security_rank'		{ L _ CmtCommand "@security_rank"	}
    '@code'			{ L _ CmtCode	 "@code"		}
    '@endcode'			{ L _ CmtCode	 "@endcode"		}

    '/n'			{ L _ PpNewline				_ }
    '/**'			{ L _ CmtStartDoc			_ }
    '*/'			{ L _ CmtEnd				_ }
    ' '				{ L _ CmtSpace				_ }

    '('				{ L _ PctLParen				_ }
    ')'				{ L _ PctRParen				_ }
    ','				{ L _ PctComma				_ }

    '[in]'			{ L _ CmtAttr				_ }

    TOKEN			{ L _ _					_ }

%nonassoc Command
%left TOKEN ' ' '(' ')' ','

%%

Comment :: { NonTerm }
Comment
:	'/**' Items '*/'				{ Fix $ DocComment [Fix (DocLine $2)] }
|	'/**' Items '/n' Lines '*/'			{ Fix $ DocComment (Fix (DocLine $2) : $4) }

Lines :: { [NonTerm] }
Lines
:							{ [] }
|	Line Lines					{ $1 : $2 }

Line :: { NonTerm }
Line
:	Items '/n'					{ Fix $ DocLine $1 }

Items :: { [NonTerm] }
Items
:							{ [] }
|	Item Items					{ $1 : $2 }

Item :: { NonTerm }
Item
:	TOKEN						{ Fix $ DocWord $1 }
|	' '						{ Fix $ DocWord $1 }
|	'('						{ Fix $ DocWord $1 }
|	')'						{ Fix $ DocWord $1 }
|	','						{ Fix $ DocWord $1 }
|	Command						{ $1 }

Command :: { NonTerm }
Command
:	'@attention'					{ Fix DocAttention }
|	'@brief'					{ Fix DocBrief }
|	'@deprecated'					{ Fix DocDeprecated }
|	'@file'						{ Fix DocFile }
|	'@extends' ' ' TOKEN				{ Fix $ DocExtends $3 }
|	'@implements' ' ' TOKEN				{ Fix $ DocImplements $3 }
|	'@note'						{ Fix DocNote }
|	'@param' '[in]' ' ' TOKEN			{ Fix $ DocParam (Just $2) $4 }
|	'@param' ' ' TOKEN				{ Fix $ DocParam Nothing $3 }
|	'@private'					{ Fix DocPrivate }
|	'@ref' ' ' TOKEN				{ Fix $ DocRef $3 }
|	'@p' ' ' TOKEN					{ Fix $ DocP $3 }
|	'@return'					{ Fix DocReturn }
|	'@retval'					{ Fix DocRetval }
|	'@section' ' ' TOKEN				{ Fix $ DocSection $3 }
|	'@subsection' ' ' TOKEN				{ Fix $ DocSubsection $3 }
|	'@see' ' ' TOKEN				{ Fix $ DocSee $3 }
|	'@security_rank' '(' TOKEN ',' ' ' TOKEN ')'	{ Fix $ DocSecurityRank $3 Nothing $6 }
|	'@security_rank' '(' TOKEN ',' ' ' TOKEN ',' ' ' TOKEN ')'	{ Fix $ DocSecurityRank $3 (Just $6) $9 }
|	'@code' Words '@endcode'			{ Fix $ DocCode $1 $2 $3 }

Words :: { [NonTerm] }
Words
:							{ [] }
|	Word Words					{ $1 : $2 }

Word :: { NonTerm }
Word
:	TOKEN					{ Fix $ DocWord $1 }
|	' '					{ Fix $ DocWord $1 }
|	'('					{ Fix $ DocWord $1 }
|	')'					{ Fix $ DocWord $1 }
|	','					{ Fix $ DocWord $1 }
|	'/n'					{ Fix $ DocWord $1 }

{
type Term = Lexeme Text
type NonTerm = Comment Term

failAt :: Lexeme Text -> String -> ParseResult a
failAt n msg =
    fail $ Text.unpack (sloc "" n) <> ": unexpected in comment: " <> describeLexeme n <> msg

parseError :: ([Lexeme Text], [String]) -> ParseResult a
parseError ([], options)  = fail $ " end of comment; expected one of " <> show options
parseError (n:_, [])	  = failAt n "; expected end of comment"
parseError (n:_, options) = failAt n $ "; expected one of " <> show options
}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Language.Cimple.Tokens
    ( LexemeClass (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data LexemeClass
    = IdConst
    | IdFuncType
    | IdStdType
    | IdSueType
    | IdVar
    | KwBreak
    | KwCase
    | KwConst
    | KwContinue
    | KwDefault
    | KwDelete
    | KwDo
    | KwElse
    | KwEnum
    | KwExtern
    | KwFor
    | KwGoto
    | KwIf
    | KwNew
    | KwReturn
    | KwSizeof
    | KwStatic
    | KwStaticAssert
    | KwStruct
    | KwSwitch
    | KwTypedef
    | KwUnion
    | KwVla
    | KwVoid
    | KwWhile
    | LitFalse
    | LitTrue
    | LitChar
    | LitInteger
    | LitString
    | LitSysInclude
    | PctAmpersand
    | PctAmpersandAmpersand
    | PctAmpersandEq
    | PctArrow
    | PctAsterisk
    | PctAsteriskEq
    | PctCaret
    | PctCaretEq
    | PctColon
    | PctComma
    | PctEllipsis
    | PctEMark
    | PctEMarkEq
    | PctEq
    | PctEqEq
    | PctGreater
    | PctGreaterEq
    | PctGreaterGreater
    | PctGreaterGreaterEq
    | PctLBrace
    | PctLBrack
    | PctLess
    | PctLessEq
    | PctLessLess
    | PctLessLessEq
    | PctLParen
    | PctMinus
    | PctMinusEq
    | PctMinusMinus
    | PctPeriod
    | PctPercent
    | PctPercentEq
    | PctPipe
    | PctPipeEq
    | PctPipePipe
    | PctPlus
    | PctPlusEq
    | PctPlusPlus
    | PctQMark
    | PctRBrace
    | PctRBrack
    | PctRParen
    | PctSemicolon
    | PctSlash
    | PctSlashEq
    | PctTilde
    | PpDefine
    | PpDefined
    | PpElif
    | PpElse
    | PpEndif
    | PpIf
    | PpIfdef
    | PpIfndef
    | PpInclude
    | PpNewline
    | PpUndef
    | CmtBlock
    | CmtIndent
    | CmtStart
    | CmtStartBlock
    | CmtStartDoc
    | CmtSpdxCopyright
    | CmtSpdxLicense
    | CmtCode
    | CmtWord
    | CmtRef
    | CmtEnd

    | Error
    | Eof
    deriving (Show, Eq, Generic, Ord)

instance FromJSON LexemeClass
instance ToJSON LexemeClass

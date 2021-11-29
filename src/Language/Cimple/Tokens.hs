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
    | IdTyVar
    | IdVar
    | KwBitmask
    | KwBreak
    | KwCase
    | KwClass
    | KwConst
    | KwContinue
    | KwDefault
    | KwDo
    | KwElse
    | KwEnum
    | KwError
    | KwEvent
    | KwExtern
    | KwFor
    | KwGoto
    | KwIf
    | KwNamespace
    | KwReturn
    | KwSizeof
    | KwStatic
    | KwStruct
    | KwSwitch
    | KwThis
    | KwTypedef
    | KwUnion
    | KwVla
    | KwVoid
    | KwWhile
    | KwWith
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
    | PpError
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

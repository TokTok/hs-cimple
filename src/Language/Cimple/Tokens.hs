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
    | KwDo
    | KwElse
    | KwEnum
    | KwExtern
    | KwFor
    | KwGoto
    | KwIf
    | KwNonNull
    | KwNullable
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
    | CmtCommand
    | CmtEndDocSection
    | CmtIndent
    | CmtStart
    | CmtStartBlock
    | CmtStartDoc
    | CmtStartDocSection
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

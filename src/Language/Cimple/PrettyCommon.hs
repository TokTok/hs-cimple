{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
module Language.Cimple.PrettyCommon where

import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import           Language.Cimple               (AssignOp (..), BinaryOp (..),
                                                Comment, CommentF (..),
                                                CommentStyle (..), Lexeme (..),
                                                LexemeClass (..), Node,
                                                NodeF (..), Nullability (..),
                                                Scope (..), UnaryOp (..),
                                                lexemeLine, lexemeText)
import           Language.Cimple.PrettyColor   (dullcyan, dullgreen, dullred,
                                                dullyellow)
import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as Term

kwBitwise         = dullgreen $ pretty "bitwise"
kwBreak           = dullred   $ pretty "break"
kwCase            = dullred   $ pretty "case"
kwConst           = dullgreen $ pretty "const"
kwContinue        = dullred   $ pretty "continue"
kwDefault         = dullred   $ pretty "default"
kwDo              = dullred   $ pretty "do"
kwElse            = dullred   $ pretty "else"
kwEnum            = dullgreen $ pretty "enum"
kwExtern          = dullgreen $ pretty "extern"
kwFor             = dullred   $ pretty "for"
kwForce           = dullgreen $ pretty "force"
kwGnuPrintf       = dullgreen $ pretty "GNU_PRINTF"
kwGoto            = dullred   $ pretty "goto"
kwIf              = dullred   $ pretty "if"
kwNonnull         = dullgreen $ pretty "_Nonnull"
kwNullable        = dullgreen $ pretty "_Nullable"
kwOwner           = dullgreen $ pretty "owner"
kwReturn          = dullred   $ pretty "return"
kwSizeof          = dullred   $ pretty "sizeof"
kwStaticAssert    = dullred   $ pretty "static_assert"
kwStatic          = dullgreen $ pretty "static"
kwStruct          = dullgreen $ pretty "struct"
kwSwitch          = dullred   $ pretty "switch"
kwTypedef         = dullgreen $ pretty "typedef"
kwUnion           = dullgreen $ pretty "union"
kwWhile           = dullred   $ pretty "while"

kwDocAttention    = dullcyan $ pretty "@attention"
kwDocBrief        = dullcyan $ pretty "@brief"
kwDocDeprecated   = dullcyan $ pretty "@deprecated"
kwDocFile         = dullcyan $ pretty "@file"
kwDocExtends      = dullcyan $ pretty "@extends"
kwDocImplements   = dullcyan $ pretty "@implements"
kwDocParam        = dullcyan $ pretty "@param"
kwDocPrivate      = dullcyan $ pretty "@private"
kwDocRef          = dullcyan $ pretty "@ref"
kwDocReturn       = dullcyan $ pretty "@return"
kwDocRetval       = dullcyan $ pretty "@retval"
kwDocP            = dullcyan $ pretty "@p"
kwDocSee          = dullcyan $ pretty "@see"
kwDocSecurityRank = dullcyan $ pretty "@security_rank"
kwDocNote         = dullcyan $ pretty "@note"
kwDocSection      = dullcyan $ pretty "@section"
kwDocSubsection   = dullcyan $ pretty "@subsection"
kwDocCode         = dullcyan $ pretty "@code"
kwDocEndCode      = dullcyan $ pretty "@endcode"

ppAssignOp :: AssignOp -> Doc AnsiStyle
ppAssignOp = \case
    AopEq     -> equals
    AopMul    -> pretty "*="
    AopDiv    -> pretty "/="
    AopPlus   -> pretty "+="
    AopMinus  -> pretty "-="
    AopBitAnd -> pretty "&="
    AopBitOr  -> pretty "|="
    AopBitXor -> pretty "^="
    AopMod    -> pretty "%="
    AopLsh    -> pretty ">>="
    AopRsh    -> pretty "<<="

ppBinaryOp :: BinaryOp -> Doc AnsiStyle
ppBinaryOp = \case
    BopNe     -> pretty "!="
    BopEq     -> pretty "=="
    BopOr     -> pretty "||"
    BopBitXor -> pretty '^'
    BopBitOr  -> pretty '|'
    BopAnd    -> pretty "&&"
    BopBitAnd -> pretty '&'
    BopDiv    -> pretty '/'
    BopMul    -> pretty '*'
    BopMod    -> pretty '%'
    BopPlus   -> pretty '+'
    BopMinus  -> pretty '-'
    BopLt     -> pretty '<'
    BopLe     -> pretty "<="
    BopLsh    -> pretty "<<"
    BopGt     -> pretty '>'
    BopGe     -> pretty ">="
    BopRsh    -> pretty ">>"

ppUnaryOp :: UnaryOp -> Doc AnsiStyle
ppUnaryOp = \case
    UopNot     -> pretty '!'
    UopNeg     -> pretty '~'
    UopMinus   -> pretty '-'
    UopAddress -> pretty '&'
    UopDeref   -> pretty '*'
    UopIncr    -> pretty "++"
    UopDecr    -> pretty "--"

cmtPrefix :: Doc AnsiStyle
cmtPrefix = dullyellow (pretty '*')

ppText :: Text -> Doc AnsiStyle
ppText = pretty . Text.unpack

ppLexeme :: Lexeme Text -> Doc AnsiStyle
ppLexeme = ppText . lexemeText

commaSep :: [Doc AnsiStyle] -> Doc AnsiStyle
commaSep = hsep . punctuate comma

plain :: Doc ann -> Doc xxx
plain = unAnnotate

renderSmart :: Float -> Int -> Doc AnsiStyle -> SimpleDocStream AnsiStyle
renderSmart ribbonFraction widthPerLine
    = layoutSmart LayoutOptions
        { layoutPageWidth = AvailablePerLine widthPerLine (realToFrac ribbonFraction) }

renderS :: Doc AnsiStyle -> String
renderS = Text.unpack . render

render :: Doc AnsiStyle -> Text
render = TL.toStrict . Term.renderLazy . renderSmart 1 120

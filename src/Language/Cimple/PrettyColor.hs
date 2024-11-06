module Language.Cimple.PrettyColor where

import           Prettyprinter
import           Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as Term

black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
    dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite, onblack,
    onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite, ondullblack,
    ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan,
    ondullwhite, underline :: Doc AnsiStyle -> Doc AnsiStyle
black         = annotate (Term.color       Term.Black)
red           = annotate (Term.color       Term.Red)
green         = annotate (Term.color       Term.Green)
yellow        = annotate (Term.color       Term.Yellow)
blue          = annotate (Term.color       Term.Blue)
magenta       = annotate (Term.color       Term.Magenta)
cyan          = annotate (Term.color       Term.Cyan)
white         = annotate (Term.color       Term.White)
dullblack     = annotate (Term.colorDull   Term.Black)
dullred       = annotate (Term.colorDull   Term.Red)
dullgreen     = annotate (Term.colorDull   Term.Green)
dullyellow    = annotate (Term.colorDull   Term.Yellow)
dullblue      = annotate (Term.colorDull   Term.Blue)
dullmagenta   = annotate (Term.colorDull   Term.Magenta)
dullcyan      = annotate (Term.colorDull   Term.Cyan)
dullwhite     = annotate (Term.colorDull   Term.White)
onblack       = annotate (Term.bgColor     Term.Black)
onred         = annotate (Term.bgColor     Term.Red)
ongreen       = annotate (Term.bgColor     Term.Green)
onyellow      = annotate (Term.bgColor     Term.Yellow)
onblue        = annotate (Term.bgColor     Term.Blue)
onmagenta     = annotate (Term.bgColor     Term.Magenta)
oncyan        = annotate (Term.bgColor     Term.Cyan)
onwhite       = annotate (Term.bgColor     Term.White)
ondullblack   = annotate (Term.bgColorDull Term.Black)
ondullred     = annotate (Term.bgColorDull Term.Red)
ondullgreen   = annotate (Term.bgColorDull Term.Green)
ondullyellow  = annotate (Term.bgColorDull Term.Yellow)
ondullblue    = annotate (Term.bgColorDull Term.Blue)
ondullmagenta = annotate (Term.bgColorDull Term.Magenta)
ondullcyan    = annotate (Term.bgColorDull Term.Cyan)
ondullwhite   = annotate (Term.bgColorDull Term.White)
underline     = annotate Term.underlined

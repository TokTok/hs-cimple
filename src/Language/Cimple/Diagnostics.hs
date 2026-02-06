{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Strict                 #-}
module Language.Cimple.Diagnostics
  ( DiagnosticLevel (..)
  , DiagnosticSpan (..)
  , Diagnostic (..)
  , IsPosition (..)
  , HasDiagnosticInfo (..)
  , CimplePos (..)
  , DiagnosticsT
  , Diagnostics
  , HasDiagnostics (..)
  , HasDiagnosticsRich (..)
  , warn
  , warnRich
  , sloc
  , lexemePos
  , nodePos
  , nodePosAndLen
  , renderPure
  , diagToText
  ) where

import           Control.Monad.State.Strict    (State)
import qualified Control.Monad.State.Strict    as State
import           Data.Fix                      (foldFix)
import           Data.Function                 (on)
import           Data.List                     (groupBy, sortBy)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Cimple.Ast           (Node)
import           Language.Cimple.DescribeAst   (HasLocation (..))
import qualified Language.Cimple.Flatten       as Flatten
import           Language.Cimple.Lexer         (AlexPosn (..), Lexeme (..),
                                                lexemeText)
import           Prettyprinter                 (Doc, align, annotate, line,
                                                pretty, vsep, (<+>))
import qualified Prettyprinter                 as PP
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold,
                                                color, colorDull)
import qualified Prettyprinter.Render.Text     as PP.Text

import qualified Data.ByteString               as BS
import qualified Data.Text.Encoding            as Text

data DiagnosticLevel = ErrorLevel | WarningLevel | NoteLevel | HelpLevel
    deriving (Show, Eq, Ord)

data DiagnosticSpan pos = DiagnosticSpan
    { spanPos    :: pos
    , spanLen    :: Int
    , spanLabels :: [Doc AnsiStyle]
    }

data Diagnostic pos = Diagnostic
    { diagPos    :: pos
    , diagLen    :: Int
    , diagLevel  :: DiagnosticLevel
    , diagMsg    :: Doc AnsiStyle
    , diagFlag   :: Maybe Text
    , diagSpans  :: [DiagnosticSpan pos]
    , diagFooter :: [(DiagnosticLevel, Doc AnsiStyle)]
    }

diagToText :: IsPosition pos => Diagnostic pos -> Text
diagToText d =
    let p = diagPos d
        msg = PP.Text.renderStrict (PP.layoutSmart PP.defaultLayoutOptions (PP.unAnnotate (diagMsg d)))
        flag = maybe "" (\f -> " [-W" <> f <> "]") (diagFlag d)
        footers = map formatFooter (diagFooter d)
        formatFooter (l, footer) =
            let pref = case l of
                    ErrorLevel   -> "error: "
                    WarningLevel -> "warning: "
                    NoteLevel    -> "note: "
                    HelpLevel    -> "help: "
            in pref <> PP.Text.renderStrict (PP.layoutSmart PP.defaultLayoutOptions (PP.unAnnotate footer))
        loc = Text.pack (posFile p) <> ":" <> Text.pack (show (posLine p)) <> ": "
        level = case diagLevel d of
            ErrorLevel   -> "error: "
            WarningLevel -> "warning: "
            NoteLevel    -> "note: "
            HelpLevel    -> "help: "
    in Text.intercalate "\n" ( (loc <> level <> msg <> flag) : map (loc <>) footers )


class IsPosition p where
    posFile   :: p -> FilePath
    posLine   :: p -> Int
    posColumn :: p -> Int
    isRealPos :: p -> Bool
    isRealPos _ = True

class IsPosition pos => HasDiagnosticInfo at pos | at -> pos where
    getDiagnosticInfo :: FilePath -> at -> (pos, Int)

data CimplePos = CimplePos
    { cimpleFile   :: FilePath
    , cimpleLine   :: Int
    , cimpleColumn :: Int
    }
    deriving (Show, Eq, Ord)

instance IsPosition CimplePos where
    posFile = cimpleFile
    posLine = cimpleLine
    posColumn = cimpleColumn

instance HasDiagnosticInfo (Lexeme Text) CimplePos where
    getDiagnosticInfo file l = (lexemePos file l, Text.length (lexemeText l))

instance HasDiagnosticInfo (Node (Lexeme Text)) CimplePos where
    getDiagnosticInfo = nodePosAndLen

lexemePos :: FilePath -> Lexeme text -> CimplePos
lexemePos file (L (AlexPn _ l c) _ _) = CimplePos file l c

nodePos :: FilePath -> Node (Lexeme text) -> CimplePos
nodePos file n =
    case foldFix Flatten.lexemes n of
        []  -> CimplePos file 0 0
        l:_ -> lexemePos file l

nodePosAndLen :: FilePath -> Node (Lexeme Text) -> (CimplePos, Int)
nodePosAndLen file n =
    case foldFix Flatten.lexemes n of
        []  -> (CimplePos file 0 0, 0)
        ls@(l:_)  ->
            let L (AlexPn start _ _) _ _ = l
                L (AlexPn end _ _) _ s = last' ls
            in (lexemePos file l, end + Text.length s - start)
  where
    last' [x]    = x
    last' (_:xs) = last' xs
    last' []     = error "nodePosAndLen: empty list"

type DiagnosticsT diags a = State diags a
type Diagnostics a = DiagnosticsT [Text] a

warn
    :: (HasLocation at, HasDiagnostics diags)
    => FilePath -> at -> Text -> DiagnosticsT diags ()
warn file l w = State.modify (addDiagnostic $ sloc file l <> ": " <> w)

warnRich
    :: (HasDiagnosticsRich diags pos)
    => Diagnostic pos -> DiagnosticsT diags ()
warnRich = State.modify . addDiagnosticRich


class HasDiagnostics a where
    addDiagnostic :: Text -> a -> a

instance HasDiagnostics [Text] where
    addDiagnostic = (:)

class HasDiagnosticsRich a pos | a -> pos where
    addDiagnosticRich :: Diagnostic pos -> a -> a

instance HasDiagnosticsRich [Diagnostic pos] pos where
    addDiagnosticRich = (:)


renderPure :: IsPosition pos => Map FilePath [Text] -> [Diagnostic pos] -> [Doc AnsiStyle]
renderPure cache = map (formatRichError cache)
  where
    formatRichError cache' (Diagnostic p len level msg flag spans footers) =
        vsep $
        [ let msgLines = map (pretty . Text.stripEnd) $ Text.lines (Text.pack (show (PP.unAnnotate msg)))
          in case msgLines of
            (l:ls) -> header <+> align (vsep ((case flag of
                Just f -> l <+> annotate (colorDull White) ("[-W" <> pretty f <> "]")
                Nothing -> l) : ls))
            []     -> case flag of
                Just f -> header <+> annotate (colorDull White) ("[-W" <> pretty f <> "]")
                Nothing -> header
        ] ++
        [ annotate (colorDull White) (pretty (replicate (width - 1) ' ') <> "-->") <+> annotate (bold <> color White) (pretty (posFile p) <> ":" <> pretty (posLine p) <> ":" <> pretty (posColumn p))
        | isRealPos p ] ++
        (if null snippet then [] else [annotate (colorDull White) (pretty (replicate width ' ') <> "|")] ++ snippet) ++
        map formatFooter footers
      where
                header = case level of
                    ErrorLevel   -> annotate (color Red    <> bold) "error:"
                    WarningLevel -> annotate (color Yellow <> bold) "warning:"
                    NoteLevel    -> annotate (color Cyan   <> bold) "note:"
                    HelpLevel    -> annotate (color Green  <> bold) "help:"

                spansToShow =
                    let primary = DiagnosticSpan p len []
                        samePos s1 s2 = isRealPos s1 && isRealPos s2 && posFile s1 == posFile s2 && posLine s1 == posLine s2 && posColumn s1 == posColumn s2
                    in if isRealPos p && all (\s -> not (samePos (spanPos s) p)) spans
                       then primary : spans
                       else spans

                width =
                    let lineNums = [posLine (spanPos s) | s <- spansToShow, isRealPos (spanPos s)]
                        maxLine = if null lineNums then 0 else maximum lineNums
                    in max 4 (length (show maxLine))

                posCompare s1 s2 =
                    let p1 = spanPos s1
                        p2 = spanPos s2
                        isPrimary f = f == posFile p
                    in compare (not (isPrimary (posFile p1))) (not (isPrimary (posFile p2))) <>
                       compare (posFile p1) (posFile p2) <>
                       compare (posLine p1) (posLine p2) <>
                       compare (posColumn p1) (posColumn p2)

                posEqLine s1 s2 =
                    let p1 = spanPos s1
                        p2 = spanPos s2
                    in posFile p1 == posFile p2 && posLine p1 == posLine p2

                posEqFile s1 s2 = posFile (spanPos s1) == posFile (spanPos s2)

                groupedByFile = groupBy posEqFile $ sortBy posCompare spansToShow

                renderFileGroup gs@(s:_) =
                    let p_g = spanPos s
                        sep = if posFile p_g /= posFile p
                              then [ annotate (colorDull White) (pretty (replicate (width - 1) ' ') <> ":::") <+> annotate (bold <> color White) (pretty (posFile p_g) <> ":" <> pretty (posLine p_g) <> ":" <> pretty (posColumn p_g))
                                   , annotate (colorDull White) (pretty (replicate width ' ') <> "|")
                                   ]
                              else []
                        lineGroups = groupBy posEqLine gs
                    in sep ++ concatMap renderGrouped lineGroups
                renderFileGroup [] = []

                snippet = concatMap renderFileGroup groupedByFile

                formatFooter (l, d) =
                    let pref = case l of
                            ErrorLevel   -> annotate (color Red    <> bold) "    = error:"
                            WarningLevel -> annotate (color Yellow <> bold) "    = warning:"
                            NoteLevel    -> annotate (color Cyan   <> bold) "    = note:"
                            HelpLevel    -> annotate (color Green  <> bold) "    = help:"
                    in pref <+> align d

                renderGrouped ss@(DiagnosticSpan sp _ _ : _) =
                    let mFileLines = if isRealPos sp then Map.lookup (posFile sp) cache' else Nothing
                        row = posLine sp
                        rowStr = show row
                        gutterStr = replicate (max 0 (width - length rowStr)) ' ' ++ rowStr ++ "|"
                        gutter = pretty gutterStr
                        lineText = case mFileLines of
                                     Just fileLines | row > 0 && row <= length fileLines ->
                                         [gutter <+> pretty (expandTabs 8 (fileLines !! (row - 1)))]
                                     _ -> []
                        -- Group spans on the same line by position and merge labels
                        uniqueSpans = map mergeLabels . groupBy ((==) `on` posAndLen') . sortBy (compare `on` posAndLen') $ ss
                        posAndLen' (DiagnosticSpan s le _) = (posColumn s, le)
                        mergeLabels [] = error "mergeLabels: empty group"
                        mergeLabels (DiagnosticSpan s le labels : samePosSpans) =
                            let allLabels = labels ++ concatMap spanLabels samePosSpans
                            in DiagnosticSpan s le allLabels

                        sourceLine = case mFileLines of
                            Just fileLines | row > 0 && row <= length fileLines -> fileLines !! (row - 1)
                            _ -> ""
                    in lineText ++ concatMap (renderSp sourceLine) uniqueSpans
                renderGrouped [] = []

                renderSp sourceLine (DiagnosticSpan sp l labels) =
                    let col = posColumn sp
                        -- 'col' is 1-based byte offset.
                        -- We need to find how many characters are before this byte offset.
                        charCol = byteToCharOffset sourceLine (col - 1)
                        -- Extract the prefix up to that character offset.
                        prefix = Text.take charCol sourceLine
                        -- Calculate visual width of that prefix (handling tabs).
                        visualCol = Text.length (expandTabs 8 prefix)

                        -- Calculate visual width of the span.
                        spanText = Text.take (byteToCharOffset (Text.drop charCol sourceLine) l) (Text.drop charCol sourceLine)
                        visualLen = Text.length (expandTabs 8 spanText)

                        padding = replicate visualCol ' '
                        underline = annotate (color Red <> bold) (pretty padding <> pretty (replicate (max 1 visualLen) '^'))
                        labelDocs = map (\lab -> line <> pretty (replicate width ' ') <> "|" <+> pretty padding <> align lab) labels
                        fullLabel = if null labels
                                    then mempty
                                    else line <> pretty (replicate width ' ') <> "|" <+> pretty padding <> "|" <> mconcat labelDocs
                    in [ pretty (replicate width ' ') <> "|" <+> underline <> fullLabel ]

expandTabs :: Int -> Text -> Text
expandTabs tabWidth = Text.pack . go 0 . Text.unpack
  where
    go _ [] = []
    go col (c:cs)
      | c == '\t' =
          let spaces = tabWidth - (col `mod` tabWidth)
          in replicate spaces ' ' ++ go (col + spaces) cs
      | otherwise = c : go (col + 1) cs

byteToCharOffset :: Text -> Int -> Int
byteToCharOffset t byteOffset =
    let bs = Text.encodeUtf8 t
    in if byteOffset >= BS.length bs
       then Text.length t
       else Text.length (Text.decodeUtf8 (BS.take byteOffset bs))

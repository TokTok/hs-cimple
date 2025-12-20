module Main (main) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           Language.Cimple        (Lexeme, Node)
import           Language.Cimple.IO     (parseText, parseUnit)
import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           Options.Applicative    (Parser, argument, execParser, fullDesc,
                                         header, help, helper, info, long, many,
                                         metavar, progDesc, str, switch)


data Options = Options
    { reparse      :: Bool
    , noTreeParser :: Bool
    , files        :: [FilePath]
    }


options :: Parser Options
options = Options
    <$> switch
        ( long "reparse"
       <> help "Reparse the output to ensure validity" )
    <*> switch
        ( long "no-tree-parser"
       <> help "Do not use the tree parser (faster, less strict)" )
    <*> many (argument str (metavar "FILES..."))


format :: Bool -> [Node (Lexeme Text)] -> Text
format color = render . maybePlain . ppTranslationUnit
  where
    maybePlain = if color then id else plain


reparseText :: Text -> IO ()
reparseText code =
    case parseText code of
        Left err -> do
            BS.putStr . Text.encodeUtf8 $ code
            fail $ "re-parsing our own pretty-printed"
                <> " output failed: " <> err
        Right _ -> return ()


processInput :: Options -> FilePath -> LBS.ByteString -> IO ()
processInput opts source input = do
    let ast = parseUnit (not $ noTreeParser opts) input
    case ast of
        Left err -> putStrLn (source <> err) >> fail "aborting after parse error"
        Right ok ->
            if reparse opts
               then reparseText $ format False ok
               else BS.putStr . Text.encodeUtf8 . format True $ ok


main :: IO ()
main = do
    opts <- execParser optsInfo
    let sources = files opts
    if null sources
        then do
            content <- LBS.fromStrict <$> BS.getContents
            processInput opts "<stdin>" content
        else mapM_ (\f -> LBS.readFile f >>= processInput opts f) sources
  where
    optsInfo = info (helper <*> options)
        ( fullDesc
       <> progDesc "Format Cimple source files"
       <> header "cimplefmt - a formatter for Cimple" )

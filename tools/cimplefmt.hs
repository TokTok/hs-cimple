module Main (main) where

import qualified Data.ByteString        as BS
import           Data.List              (isPrefixOf)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Language.Cimple        (Lexeme, Node)
import           Language.Cimple.IO     (parseFile, parseText)
import           Language.Cimple.Pretty (ppTranslationUnit)
import           System.Environment     (getArgs)


format :: [Node (Lexeme Text)] -> Text
format = Text.pack . show . ppTranslationUnit


reparseText :: Text -> IO ()
reparseText code =
    case parseText code of
        Left err -> do
            BS.putStr . Text.encodeUtf8 $ code
            fail $ "re-parsing our own pretty-printed"
                <> " output failed: " <> err
        Right ok' -> BS.putStr . Text.encodeUtf8 . format $ ok'


processFile :: [String] -> FilePath -> IO ()
processFile flags source = do
    putStrLn $ "Processing " ++ source
    ast <- parseFile source
    case ast of
        Left err -> fail err
        Right (_, ok) ->
            if "--no-reparse" `elem` flags
               then BS.putStr . Text.encodeUtf8 . format $ ok
               else reparseText $ format ok


main :: IO ()
main = do
    (flags, files) <- span ("-" `isPrefixOf`) <$> getArgs
    mapM_ (processFile flags) files

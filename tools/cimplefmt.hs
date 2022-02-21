module Main (main) where

import qualified Data.ByteString        as BS
import           Data.List              (isPrefixOf)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           Language.Cimple        (Lexeme, Node)
import           Language.Cimple.IO     (parseFile, parseText)
import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           System.Environment     (getArgs)


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


processFile :: [String] -> FilePath -> IO ()
processFile flags source = do
    putStrLn $ "Processing " ++ source
    ast <- parseFile source
    case ast of
        Left err -> fail err
        Right (_, ok) ->
            if "--reparse" `elem` flags
               then reparseText $ format False ok
               else BS.putStr . Text.encodeUtf8 . format True $ ok


main :: IO ()
main = do
    (flags, files) <- span ("-" `isPrefixOf`) <$> getArgs
    mapM_ (processFile flags) files

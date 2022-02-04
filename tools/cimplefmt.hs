module Main (main) where

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Language.Cimple.IO     (parseFile)
import           Language.Cimple.Pretty (ppTranslationUnit)
import           System.Environment     (getArgs)


processFile :: FilePath -> IO ()
processFile source = do
    putStrLn $ "Processing " ++ source
    ast <- parseFile source
    case ast of
        Left err -> fail err
        Right ok ->
            BS.putStr
            . Text.encodeUtf8
            . Text.pack
            . show
            . ppTranslationUnit
            . snd
            $ ok


main :: IO ()
main = do
    args <- getArgs
    mapM_ processFile args

module Main (main) where

import           Language.Cimple.IO     (parseFile)
import           Language.Cimple.Pretty (ppTranslationUnit)
import           System.Environment     (getArgs)


processFile :: FilePath -> IO ()
processFile source = do
    putStrLn $ "Processing " ++ source
    ast <- parseFile source
    case ast of
        Left  err -> fail err
        Right ok  -> print $ ppTranslationUnit $ snd ok


main :: IO ()
main = do
    args <- getArgs
    mapM_ processFile args

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Language.Cimple.IO (parseFile)
import           System.Environment (getArgs)
import           Text.Groom         (groom)


processFile :: FilePath -> IO ()
processFile source = do
    putStrLn $ "Processing " ++ source
    ast <- parseFile source
    case ast of
        Left err -> fail err
        Right ok -> putStrLn $ groom ok


main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> processFile src
    _     -> fail "Usage: dump-ast <file.c>"

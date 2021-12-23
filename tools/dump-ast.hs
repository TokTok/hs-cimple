{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Language.Cimple.IO (parseFiles)
import           System.Environment (getArgs)
import           Text.Groom         (groom)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> fail "Usage: dump-ast [FILE]..."
    srcs ->
      parseFiles srcs
      >>= getRight
      >>= mapM_ (putStrLn . groom)
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok

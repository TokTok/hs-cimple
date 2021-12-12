module Main (main) where

import           Language.Cimple.IO      (parseProgram)
import qualified Language.Cimple.Program as Program
import           System.Environment      (getArgs)
import           Text.Groom              (groom)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> fail "Usage: include-graph [FILE]..."
    srcs ->
      parseProgram srcs
      >>= getRight
      >>= mapM_ (putStrLn . groom) . Program.toList
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok

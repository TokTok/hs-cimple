{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Language.Cimple.IO         (parseFiles)
import           System.Environment         (getArgs)
import           Text.Groom                 (groom)


main :: IO ()
main = do
  args <- getArgs
  case args of
    "--json" : srcs@(_:_) ->
      parseFiles srcs
      >>= getRight
      >>= LBS.putStrLn . encode . map (\(file, nodes) -> object ["file" .= file, "nodes" .= nodes])
    srcs@(_:_) ->
      parseFiles srcs
      >>= getRight
      >>= mapM_ (putStrLn . groom)
    _ -> fail "Usage: dump-ast [--json] FILE..."
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok

module Main (main) where

import qualified Data.ByteString    as BS
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Language.Cimple    (alexScanTokens)
import           System.Environment (getArgs)
import           Text.Groom         (groom)

processFile :: FilePath -> IO ()
processFile source = do
    putStrLn $ "Processing " ++ source
    contents <- Text.unpack . Text.decodeUtf8 <$> BS.readFile source
    case alexScanTokens contents of
        Left err -> fail err
        Right ok -> putStrLn $ groom ok

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> processFile src
    _     -> fail "Usage: dump-tokens <file.c>"

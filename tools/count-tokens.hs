{-# LANGUAGE Strict #-}
module Main (main) where

import           Data.Bifunctor       (bimap)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import           Language.Cimple      (Alex, Lexeme (..), LexemeClass (..),
                                       alexMonadScan, runAlex)
import           System.Environment   (getArgs)


countTokens :: LBS.ByteString -> Either String (Int, Int)
countTokens str = runAlex str $ loop 0 0
  where
    loop :: Int -> Int -> Alex (Int, Int)
    loop toks len = do
        (L _ c t) <- alexMonadScan
        if c == Eof
            then return (toks, len)
            else loop (toks + 1) (len + Text.length t)

processFile :: FilePath -> IO (Int, Int)
processFile source = do
    contents <- LBS.readFile source
    case countTokens contents of
        Left err -> fail err
        Right ok -> return ok

processFiles :: [FilePath] -> IO (Int, Int)
processFiles = fmap (bimap sum sum . unzip) . mapM processFile

main :: IO ()
main = do
    sources <- getArgs
    start <- getCurrentTime
    (toks, len) <- processFiles sources
    end <- getCurrentTime
    putStrLn $ "Tokenised " <> show (length sources) <> " sources into "
        <> show toks <> " lexemes (" <> show len <> " bytes) in "
        <> show (diffUTCTime end start) <> " ("
        <> show (fromIntegral len / 1024 / 1024 / diffUTCTime end start) <> " MiB/s)"

{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}
module Language.Cimple.IO
    ( parseFile
    , parseFiles
    , parseProgram
    , parseText
    ) where

import           Control.Monad.State.Lazy (State, get, put, runState)
import qualified Data.ByteString          as BS
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Language.Cimple.AST      (Node (..))
import           Language.Cimple.Lexer    (Lexeme, runAlex)
import           Language.Cimple.Parser   (parseCimple)
import           Language.Cimple.Program  (Program, TranslationUnit)
import qualified Language.Cimple.Program  as Program

type CacheState a = State (Map String Text) a

cacheText :: String -> CacheState Text
cacheText s = do
    m <- get
    case Map.lookup s m of
        Nothing -> do
            let text = Text.pack s
            put $ Map.insert s text m
            return text
        Just text ->
            return text


process :: [Node (Lexeme String)] -> [Node (Lexeme Text)]
process stringAst =
    fst $ runState (mapM (mapM (mapM cacheText)) stringAst) Map.empty


parseText :: Text -> Either String [Node (Lexeme Text)]
parseText contents =
    process <$> res
  where
    res :: Either String [Node (Lexeme String)]
    res = runAlex (Text.unpack contents) parseCimple


parseFile :: FilePath -> IO (Either String (TranslationUnit Text))
parseFile source = do
    putStrLn $ "Processing " ++ source
    fmap (source,) . parseText . Text.decodeUtf8 <$> BS.readFile source


parseFiles :: [FilePath] -> IO (Either String [TranslationUnit Text])
parseFiles sources = sequenceA <$> traverse parseFile sources


parseProgram :: [FilePath] -> IO (Either String (Program Text))
parseProgram sources = fmap Program.fromList <$> parseFiles sources

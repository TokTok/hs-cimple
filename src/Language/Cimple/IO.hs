{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Language.Cimple.IO
    ( parseExpr
    , parseFile
    , parseFiles
    , parseProgram
    , parseStmt
    , parseText
    ) where

import           Control.Monad                   ((>=>))
import qualified Control.Monad.Parallel          as P
import           Control.Monad.State.Strict      (State, evalState, get, put)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as Text
import           Language.Cimple.Ast             (Node)
import           Language.Cimple.Lexer           (Alex, Lexeme, runAlex)
import           Language.Cimple.MapAst          (TextActions, mapAst,
                                                  textActions)
import qualified Language.Cimple.Parser          as Parser
import qualified Language.Cimple.ParseResult     as ParseResult
import           Language.Cimple.Program         (Program)
import qualified Language.Cimple.Program         as Program
import           Language.Cimple.TranslationUnit (TranslationUnit)
import qualified Language.Cimple.TreeParser      as TreeParser

type TextNode = Node (Lexeme Text)

cacheText :: [TextNode] -> [TextNode]
cacheText textAst =
    evalState (mapAst cacheActions textAst) Map.empty
  where
    cacheActions :: TextActions (State (Map Text Text)) Text Text
    cacheActions = textActions $ \s -> do
        m <- get
        case Map.lookup s m of
            Nothing -> do
                put $ Map.insert s s m
                return s
            Just text ->
                return text


runText :: Alex a -> Text -> Either String a
runText f = flip runAlex f . LBS.fromStrict . Text.encodeUtf8

parseExpr :: Text -> Either String TextNode
parseExpr = runText Parser.parseStmt

parseStmt :: Text -> Either String TextNode
parseStmt = runText Parser.parseStmt

parseText :: Text -> Either String [TextNode]
parseText = fmap cacheText . runText Parser.parseTranslationUnit

parseBytes :: LBS.ByteString -> Either String [TextNode]
parseBytes = flip runAlex Parser.parseTranslationUnit

parseBytesPedantic :: LBS.ByteString -> Either String [TextNode]
parseBytesPedantic = parseBytes >=> ParseResult.toEither . TreeParser.parseTranslationUnit


parseFile :: FilePath -> IO (Either String (TranslationUnit Text))
parseFile source =
    addSource . parseBytesPedantic <$> LBS.readFile source
  where
    -- Add source filename to the error message, if any.
    addSource (Left err) = Left $ source <> err
    -- If there's no error message, record the source filename in the returned
    -- TranslationUnit.
    addSource (Right ok) = Right (source, ok)


parseFiles :: [FilePath] -> IO (Either String [TranslationUnit Text])
parseFiles sources = sequenceA <$> P.mapM parseFile sources


parseProgram :: [FilePath] -> IO (Either String (Program Text))
parseProgram sources = (>>= Program.fromList) <$> parseFiles sources

{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Language.Cimple.IO
    ( parseFile
    , parseFiles
    , parseProgram
    , parseText
    ) where

import           Control.Monad                   ((>=>))
import qualified Control.Monad.Parallel          as P
import           Control.Monad.State.Strict      (State, evalState, get, put)
import qualified Data.ByteString                 as BS
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Language.Cimple.Ast             (Node)
import           Language.Cimple.Lexer           (Lexeme, runAlex)
import qualified Language.Cimple.Parser          as Parser
import           Language.Cimple.Program         (Program)
import qualified Language.Cimple.Program         as Program
import           Language.Cimple.TranslationUnit (TranslationUnit)
import           Language.Cimple.TraverseAst     (TextActions, textActions,
                                                  traverseAst)
import qualified Language.Cimple.TreeParser      as TreeParser

type StringNode = Node (Lexeme String)
type TextNode = Node (Lexeme Text)

toTextAst :: [StringNode] -> [TextNode]
toTextAst stringAst =
    evalState (traverseAst cacheActions stringAst) Map.empty
  where
    cacheActions :: TextActions (State (Map String Text)) String Text
    cacheActions = textActions $ \s -> do
        m <- get
        case Map.lookup s m of
            Nothing -> do
                let text = Text.pack s
                put $ Map.insert s text m
                return text
            Just text ->
                return text


parseText :: Text -> Either String [TextNode]
parseText contents =
    toTextAst <$> runAlex (Text.unpack contents) Parser.parseTranslationUnit

parseTextPedantic :: Text -> Either String [TextNode]
parseTextPedantic =
    parseText >=> TreeParser.toEither . TreeParser.parseTranslationUnit


parseFile :: FilePath -> IO (Either String (TranslationUnit Text))
parseFile source =
    addSource . parseTextPedantic . Text.decodeUtf8 <$> BS.readFile source
  where
    -- Add source filename to the error message, if any.
    addSource (Left err) = Left $ source <> ":" <> err
    -- If there's no error message, record the source filename in the returned
    -- TranslationUnit.
    addSource (Right ok) = Right (source, ok)


parseFiles' :: [FilePath] -> IO (Either String [TranslationUnit Text])
parseFiles' sources = sequenceA <$> P.mapM parseFile sources


parseFiles :: [FilePath] -> IO (Either String [TranslationUnit Text])
parseFiles sources = fmap Program.toList . (>>= Program.fromList) <$> parseFiles' sources


parseProgram :: [FilePath] -> IO (Either String (Program Text))
parseProgram sources = (>>= Program.fromList) <$> parseFiles' sources

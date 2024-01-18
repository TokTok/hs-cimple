{-# LANGUAGE OverloadedStrings #-}
module Language.Cimple.ArbitrarySpec (spec) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Language.Alex.Parser   (parseAlex)
import           Language.Cimple        (alexScanTokens, lexemeClass)
import           Language.Cimple.IO     (parseText)
import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import qualified Language.Happy         as Happy
import           Language.Happy.Linker  (Lexer, defaultLinkConfig,
                                         generateChecked)
import           System.Directory       (doesFileExist)
import           Test.Hspec             (Spec, describe, it, runIO, shouldBe)
import           Test.QuickCheck        (forAll, suchThatMap)

-- | Finds a source file from Bazel's or cabal's working directory.
sourceFile :: FilePath -> IO FilePath
sourceFile path = do
    let bazelPath = "hs-cimple/" ++ path
    inBazel <- doesFileExist bazelPath
    return $ if inBazel then bazelPath else path

cimpleLexer :: Lexer
cimpleLexer =
    either (const Nothing) (Just . map (Text.pack . show . lexemeClass))
    . alexScanTokens . LBS.fromStrict . Text.encodeUtf8

spec :: Spec
spec = describe "arbitrary programs" $ do
    alex <- runIO $ either error id . parseAlex . Text.decodeUtf8 . LBS.toStrict <$> (LBS.readFile =<< sourceFile "src/Language/Cimple/Lexer.x")
    grammar <- runIO $ LBS.readFile =<< sourceFile "src/Language/Cimple/Parser.y"
    let happy = either error id $ Happy.runAlex grammar Happy.parseGrammar
        -- The grammar doesn't have the checks in the parser's actions and the
        -- tree pass, so we drop the programs they reject.
        programs = generateChecked defaultLinkConfig alex cimpleLexer "TranslationUnit" happy
            `suchThatMap` \code -> (,) code <$> either (const Nothing) Just (parseText code)

    it "print the same after parsing what they printed" $
        forAll programs $ \(_, ast) ->
            let printed = pp ast in pp <$> parseText printed `shouldBe` Right printed
  where
    pp = render . plain . ppTranslationUnit

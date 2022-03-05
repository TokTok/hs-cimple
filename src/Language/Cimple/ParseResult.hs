{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Cimple.ParseResult
    ( ParseResult
    , toEither
    ) where

newtype ParseResult a = ParseResult { toEither :: Either String a }
    deriving (Functor, Applicative, Monad)

instance MonadFail ParseResult where
    fail = ParseResult . Left

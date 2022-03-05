{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
module Language.Cimple.Flatten (lexemes) where

import           Data.Fix            (Fix (..))
import           Data.Maybe          (maybeToList)
import           GHC.Generics
import           Language.Cimple.Ast (AssignOp, BinaryOp, CommentF,
                                      CommentStyle, LiteralType, NodeF (..),
                                      Scope, UnaryOp)

class Concats t a where
    concats :: t -> [a]

    default concats :: (Generic t, GenConcats (Rep t) a) => t -> [a]
    concats = gconcats . from

class GenConcats f a where
    gconcats :: f p -> [a]

instance GenConcats U1 a where
    gconcats U1 = []

instance (GenConcats f a, GenConcats g a) => GenConcats (f :+: g) a where
    gconcats (L1 x) = gconcats x
    gconcats (R1 x) = gconcats x

instance (GenConcats f a, GenConcats g a) => GenConcats (f :*: g) a where
    gconcats (x :*: y) = gconcats x ++ gconcats y

instance GenConcats f a => GenConcats (M1 i t f) a where
    gconcats (M1 x) = gconcats x

class GenConcatsFlatten t a where
    gconcatsFlatten :: t -> [a]

instance GenConcatsFlatten UnaryOp      a where gconcatsFlatten = const []
instance GenConcatsFlatten BinaryOp     a where gconcatsFlatten = const []
instance GenConcatsFlatten AssignOp     a where gconcatsFlatten = const []
instance GenConcatsFlatten Scope        a where gconcatsFlatten = const []
instance GenConcatsFlatten CommentStyle a where gconcatsFlatten = const []
instance GenConcatsFlatten LiteralType  a where gconcatsFlatten = const []

instance GenConcatsFlatten b a => GenConcatsFlatten (Maybe b) a where
    gconcatsFlatten = gconcatsFlatten . maybeToList

instance {-# OVERLAPPING #-} GenConcatsFlatten a a where
    gconcatsFlatten = pure

instance {-# OVERLAPPABLE #-} GenConcatsFlatten b a => GenConcatsFlatten [b] a where
    gconcatsFlatten = concatMap gconcatsFlatten

instance GenConcatsFlatten (Fix (CommentF a)) a where
    gconcatsFlatten = error "TODO: gconcatsFlatten for CommentF"

instance GenConcatsFlatten t a => GenConcats (Rec0 t) a where
    gconcats (K1 x) = gconcatsFlatten x

-- Uses the default signature, which delegates to the generic stuff
instance Concats (NodeF a [a]) a
instance Concats (CommentF a [a]) a

lexemes :: NodeF lexeme [lexeme] -> [lexeme]
lexemes = concats

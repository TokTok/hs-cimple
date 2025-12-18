{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeOperators         #-}
module Language.Cimple.Flatten (Concats (..), lexemes) where

import           Data.Fix            (Fix (..))
import           Data.Maybe          (maybeToList)
import           GHC.Generics
import           Language.Cimple.Ast (AssignOp, BinaryOp, CommentF (..),
                                      CommentStyle, LiteralType, NodeF (..),
                                      Nullability, Scope, UnaryOp)

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
instance GenConcatsFlatten Nullability  a where gconcatsFlatten = const []

instance (GenConcatsFlatten b a, GenConcatsFlatten c a) => GenConcatsFlatten (b, c) a where
    gconcatsFlatten (x, y) = gconcatsFlatten x ++ gconcatsFlatten y

instance GenConcatsFlatten b a => GenConcatsFlatten (Maybe b) a where
    gconcatsFlatten = gconcatsFlatten . maybeToList

instance {-# OVERLAPPING #-} GenConcatsFlatten a a where
    gconcatsFlatten = pure

instance {-# OVERLAPPABLE #-} GenConcatsFlatten b a => GenConcatsFlatten [b] a where
    gconcatsFlatten = concatMap gconcatsFlatten

instance GenConcatsFlatten (Fix (CommentF a)) a where
    -- TODO(iphydf): Figure out how to write this using Generics.
    gconcatsFlatten (Fix DocFile) = []
    gconcatsFlatten (Fix DocPrivate) = []
    gconcatsFlatten (Fix DocAttention) = []
    gconcatsFlatten (Fix (DocBrief)) = []
    gconcatsFlatten (Fix (DocComment x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocDeprecated)) = []
    gconcatsFlatten (Fix (DocExtends x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocImplements x)) = gconcatsFlatten x
    gconcatsFlatten (Fix DocNote) = []
    gconcatsFlatten (Fix (DocLine x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocCode b x e)) = concat [gconcatsFlatten b, gconcatsFlatten x, gconcatsFlatten e]
    gconcatsFlatten (Fix (DocParam a p)) = concat [gconcatsFlatten a, gconcatsFlatten p]
    gconcatsFlatten (Fix (DocP x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocRef x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocReturn)) = []
    gconcatsFlatten (Fix (DocRetval)) = []
    gconcatsFlatten (Fix (DocSection s)) = [s]
    gconcatsFlatten (Fix (DocSee r)) = [r]
    gconcatsFlatten (Fix (DocSecurityRank s p r)) = s : maybeToList p ++ [r]
    gconcatsFlatten (Fix (DocSubsection s)) = [s]
    gconcatsFlatten (Fix (DocWord x)) = [x]

instance GenConcatsFlatten t a => GenConcats (Rec0 t) a where
    gconcats (K1 x) = gconcatsFlatten x

-- Uses the default signature, which delegates to the generic stuff
instance Concats (NodeF a [a]) a
instance Concats (CommentF a [a]) a

lexemes :: NodeF lexeme [lexeme] -> [lexeme]
lexemes = concats

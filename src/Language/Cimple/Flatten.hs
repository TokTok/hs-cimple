{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeOperators         #-}
module Language.Cimple.Flatten (lexemes) where

import           Data.Fix            (Fix (..))
import           Data.Maybe          (maybeToList)
import           GHC.Generics
import           Language.Cimple.Ast (AssignOp, BinaryOp, CommentF (..),
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
    -- TODO(iphydf): Figure out how to write this using Generics.
    gconcatsFlatten (Fix DocNewline) = []
    gconcatsFlatten (Fix DocPrivate) = []
    gconcatsFlatten (Fix (DocAssignOp _ l r)) = concatMap gconcatsFlatten [l, r]
    gconcatsFlatten (Fix (DocAttention x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocBinaryOp _ l r)) = concatMap gconcatsFlatten [l, r]
    gconcatsFlatten (Fix (DocBrief x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocColon x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocComment x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocDeprecated x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocExtends x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocImplements x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocLine x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocList x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocLParen x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocOLItem i x)) = i : gconcatsFlatten x
    gconcatsFlatten (Fix (DocParagraph x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocParam a p x)) = concat [gconcatsFlatten a, gconcatsFlatten p, gconcatsFlatten x]
    gconcatsFlatten (Fix (DocP x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocRef x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocReturn x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocRetval r x)) = r : gconcatsFlatten x
    gconcatsFlatten (Fix (DocRParen x)) = gconcatsFlatten x
    gconcatsFlatten (Fix (DocSee r x)) = r : gconcatsFlatten x
    gconcatsFlatten (Fix (DocSentence x p)) = gconcatsFlatten x ++ [p]
    gconcatsFlatten (Fix (DocULItem i x)) = gconcatsFlatten i ++ gconcatsFlatten x
    gconcatsFlatten (Fix (DocWord x)) = [x]

instance GenConcatsFlatten t a => GenConcats (Rec0 t) a where
    gconcats (K1 x) = gconcatsFlatten x

-- Uses the default signature, which delegates to the generic stuff
instance Concats (NodeF a [a]) a
instance Concats (CommentF a [a]) a

lexemes :: NodeF lexeme [lexeme] -> [lexeme]
lexemes = concats

{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeOperators      #-}
module Language.Cimple.Annot
    ( AnnotF (..)
    , AnnotNode
    , addAnnot
    , removeAnnot
    ) where

import           Data.Fix                     (Fix, hoistFix)
import           Data.Functor.Classes         (Eq1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           Data.Functor.Compose         (Compose (..))
import           GHC.Generics                 (Generic, Generic1)
import           Language.Cimple.Ast          (Node, NodeF)

data AnnotF attr a = Annot { attr :: attr, unAnnot :: a }
    deriving (Functor, Generic, Generic1)
    deriving (Show1, Read1, Eq1) via FunctorClassesDefault (AnnotF attr)

type AnnotNode lexeme = Fix (AnnotF () `Compose` NodeF lexeme)

addAnnot :: Node lexeme -> AnnotNode lexeme
addAnnot = hoistFix $ Compose . Annot ()

removeAnnot :: AnnotNode lexeme -> Node lexeme
removeAnnot = hoistFix $ unAnnot . getCompose

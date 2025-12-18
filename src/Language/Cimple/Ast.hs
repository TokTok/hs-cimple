{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict            #-}
module Language.Cimple.Ast
    ( AssignOp (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , LiteralType (..)
    , Node, NodeF (..)
    , Scope (..)
    , CommentStyle (..)
    , Comment
    , CommentF (..)
    , Nullability (..)
    , getNodeId
    ) where

import           Data.Aeson                   (FromJSON, FromJSON1, ToJSON,
                                               ToJSON1)
import           Data.Bifunctor               (Bifunctor (..))
import           Data.Fix                     (Fix (..))
import           Data.Functor.Classes         (Eq1, Ord1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           Data.Hashable                (Hashable (..))
import           Data.Hashable.Lifted         (Hashable1)
import           GHC.Generics                 (Generic, Generic1)

getNodeId :: Hashable a => Node a -> Int
getNodeId = hash

data NodeF lexeme a
    -- Preprocessor
    = PreprocInclude lexeme
    | PreprocDefine lexeme
    | PreprocDefineConst lexeme a
    | PreprocDefineMacro lexeme [a] a
    | PreprocIf a [a] a
    | PreprocIfdef lexeme [a] a
    | PreprocIfndef lexeme [a] a
    | PreprocElse [a]
    | PreprocElif a [a] a
    | PreprocUndef lexeme
    | PreprocDefined lexeme
    | PreprocScopedDefine a [a] a
    | MacroBodyStmt a
    | MacroBodyFunCall a
    | MacroParam lexeme
    | StaticAssert a lexeme
    -- Comments
    | LicenseDecl lexeme [a]
    | CopyrightDecl lexeme (Maybe lexeme) [lexeme]
    | Comment CommentStyle lexeme [lexeme] lexeme
    | CommentSection a [a] a
    | CommentSectionEnd lexeme
    | Commented a a
    | CommentInfo (Comment lexeme)
    -- Namespace-like blocks
    | ExternC [a]
    -- An inferred coherent block of nodes, printed without empty lines
    -- between them.
    | Group [a]
    -- Statements
    | CompoundStmt [a]
    | Break
    | Goto lexeme
    | Continue
    | Return (Maybe a)
    | SwitchStmt a [a]
    | IfStmt a a (Maybe a)
    | ForStmt a a a a
    | WhileStmt a a
    | DoWhileStmt a a
    | Case a a
    | Default a
    | Label lexeme a
    | ExprStmt a
    -- Variable declarations
    | VLA a lexeme a
    | VarDeclStmt a (Maybe a)
    | VarDecl a lexeme [a]
    | DeclSpecArray (Maybe a)
    | ArrayDim Nullability a
    -- Expressions
    | InitialiserList [a]
    | UnaryExpr UnaryOp a
    | BinaryExpr a BinaryOp a
    | TernaryExpr a a a
    | AssignExpr a AssignOp a
    | ParenExpr a
    | CastExpr a a
    | CompoundExpr a a -- DEPRECATED
    | CompoundLiteral a a
    | SizeofExpr a
    | SizeofType a
    | LiteralExpr LiteralType lexeme
    | VarExpr lexeme
    | MemberAccess a lexeme
    | PointerAccess a lexeme
    | ArrayAccess a a
    | FunctionCall a [a]
    | CommentExpr a a
    -- Type definitions
    | EnumConsts (Maybe lexeme) [a]
    | EnumDecl lexeme [a] lexeme
    | Enumerator lexeme (Maybe a)
    | AggregateDecl a
    | Typedef a lexeme
    | TypedefFunction a
    | Struct lexeme [a]
    | Union lexeme [a]
    | MemberDecl a (Maybe lexeme)
    | TyBitwise a
    | TyForce a
    | TyConst a
    | TyOwner a
    | TyNonnull a
    | TyNullable a
    | TyPointer a
    | TyStruct lexeme
    | TyUnion lexeme
    | TyFunc lexeme
    | TyStd lexeme
    | TyUserDefined lexeme
    -- Functions
    | AttrPrintf lexeme lexeme a
    | FunctionDecl Scope a
    | FunctionDefn Scope a a
    | FunctionPrototype a lexeme [a]
    | CallbackDecl lexeme lexeme
    | Ellipsis
    | NonNull [lexeme] [lexeme] a
    | NonNullParam a
    | NullableParam a
    -- Constants
    | ConstDecl a lexeme
    | ConstDefn Scope a lexeme a
    deriving (Show, Read, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1, Ord1) via FunctorClassesDefault (NodeF lexeme)

instance Bifunctor NodeF where
    bimap f g n = case n of
        PreprocInclude l -> PreprocInclude (f l)
        PreprocDefine l -> PreprocDefine (f l)
        PreprocDefineConst l a -> PreprocDefineConst (f l) (g a)
        PreprocDefineMacro l as a -> PreprocDefineMacro (f l) (map g as) (g a)
        PreprocIf a as a' -> PreprocIf (g a) (map g as) (g a')
        PreprocIfdef l as a -> PreprocIfdef (f l) (map g as) (g a)
        PreprocIfndef l as a -> PreprocIfndef (f l) (map g as) (g a)
        PreprocElse as -> PreprocElse (map g as)
        PreprocElif a as a' -> PreprocElif (g a) (map g as) (g a')
        PreprocUndef l -> PreprocUndef (f l)
        PreprocDefined l -> PreprocDefined (f l)
        PreprocScopedDefine a as a' -> PreprocScopedDefine (g a) (map g as) (g a')
        MacroBodyStmt a -> MacroBodyStmt (g a)
        MacroBodyFunCall a -> MacroBodyFunCall (g a)
        MacroParam l -> MacroParam (f l)
        StaticAssert a l -> StaticAssert (g a) (f l)
        LicenseDecl l as -> LicenseDecl (f l) (map g as)
        CopyrightDecl l ml ls -> CopyrightDecl (f l) (fmap f ml) (map f ls)
        Comment s l ls l' -> Comment s (f l) (map f ls) (f l')
        CommentSection a as a' -> CommentSection (g a) (map g as) (g a')
        CommentSectionEnd l -> CommentSectionEnd (f l)
        Commented a a' -> Commented (g a) (g a')
        CommentInfo c -> CommentInfo (mapCommentLexeme f c)
        ExternC as -> ExternC (map g as)
        Group as -> Group (map g as)
        CompoundStmt as -> CompoundStmt (map g as)
        Break -> Break
        Goto l -> Goto (f l)
        Continue -> Continue
        Return ma -> Return (fmap g ma)
        SwitchStmt a as -> SwitchStmt (g a) (map g as)
        IfStmt a a' ma'' -> IfStmt (g a) (g a') (fmap g ma'')
        ForStmt a a' a'' a''' -> ForStmt (g a) (g a') (g a'') (g a''')
        WhileStmt a as -> WhileStmt (g a) (g as)
        DoWhileStmt a a' -> DoWhileStmt (g a) (g a')
        Case a a' -> Case (g a) (g a')
        Default a -> Default (g a)
        Label l a -> Label (f l) (g a)
        ExprStmt a -> ExprStmt (g a)
        VLA a l a' -> VLA (g a) (f l) (g a')
        VarDeclStmt a ma -> VarDeclStmt (g a) (fmap g ma)
        VarDecl a l as -> VarDecl (g a) (f l) (map g as)
        DeclSpecArray ma -> DeclSpecArray (fmap g ma)
        ArrayDim nullability a -> ArrayDim nullability (g a)
        InitialiserList as -> InitialiserList (map g as)
        UnaryExpr u a -> UnaryExpr u (g a)
        BinaryExpr a b a' -> BinaryExpr (g a) b (g a')
        TernaryExpr a a' a'' -> TernaryExpr (g a) (g a') (g a'')
        AssignExpr a as a' -> AssignExpr (g a) as (g a')
        ParenExpr a -> ParenExpr (g a)
        CastExpr a a' -> CastExpr (g a) (g a')
        CompoundExpr a a' -> CompoundExpr (g a) (g a')
        CompoundLiteral a a' -> CompoundLiteral (g a) (g a')
        SizeofExpr a -> SizeofExpr (g a)
        SizeofType a -> SizeofType (g a)
        LiteralExpr t l -> LiteralExpr t (f l)
        VarExpr l -> VarExpr (f l)
        MemberAccess a l -> MemberAccess (g a) (f l)
        PointerAccess a l -> PointerAccess (g a) (f l)
        ArrayAccess a a' -> ArrayAccess (g a) (g a')
        FunctionCall a as -> FunctionCall (g a) (map g as)
        CommentExpr a a' -> CommentExpr (g a) (g a')
        EnumConsts ml as -> EnumConsts (fmap f ml) (map g as)
        EnumDecl l as l' -> EnumDecl (f l) (map g as) (f l')
        Enumerator l ma -> Enumerator (f l) (fmap g ma)
        AggregateDecl a -> AggregateDecl (g a)
        Typedef a l -> Typedef (g a) (f l)
        TypedefFunction a -> TypedefFunction (g a)
        Struct l as -> Struct (f l) (map g as)
        Union l as -> Union (f l) (map g as)
        MemberDecl a ml -> MemberDecl (g a) (fmap f ml)
        TyBitwise a -> TyBitwise (g a)
        TyForce a -> TyForce (g a)
        TyConst a -> TyConst (g a)
        TyOwner a -> TyOwner (g a)
        TyNonnull a -> TyNonnull (g a)
        TyNullable a -> TyNullable (g a)
        TyPointer a -> TyPointer (g a)
        TyStruct l -> TyStruct (f l)
        TyUnion l -> TyUnion (f l)
        TyFunc l -> TyFunc (f l)
        TyStd l -> TyStd (f l)
        TyUserDefined l -> TyUserDefined (f l)
        AttrPrintf l l' a -> AttrPrintf (f l) (f l') (g a)
        FunctionDecl s a -> FunctionDecl s (g a)
        FunctionDefn s a a' -> FunctionDefn s (g a) (g a')
        FunctionPrototype a l as -> FunctionPrototype (g a) (f l) (map g as)
        CallbackDecl l l' -> CallbackDecl (f l) (f l')
        Ellipsis -> Ellipsis
        NonNull ls ls' a -> NonNull (map f ls) (map f ls') (g a)
        NonNullParam a -> NonNullParam (g a)
        NullableParam a -> NullableParam (g a)
        ConstDecl a l -> ConstDecl (g a) (f l)
        ConstDefn s a l a' -> ConstDefn s (g a) (f l) (g a')

type Node lexeme = Fix (NodeF lexeme)

instance FromJSON lexeme => FromJSON1 (NodeF lexeme)
instance ToJSON lexeme => ToJSON1 (NodeF lexeme)
instance Hashable lexeme => Hashable1 (NodeF lexeme)

data CommentF lexeme a
    = DocComment [a]

    | DocAttention
    | DocBrief
    | DocDeprecated
    | DocExtends lexeme
    | DocFile
    | DocImplements lexeme
    | DocNote
    | DocParam (Maybe lexeme) lexeme
    | DocReturn
    | DocRetval
    | DocSection lexeme
    | DocSecurityRank lexeme (Maybe lexeme) lexeme
    | DocSee lexeme
    | DocSubsection lexeme

    | DocPrivate

    | DocLine [a]
    | DocCode lexeme [a] lexeme

    | DocWord lexeme
    | DocRef lexeme
    | DocP lexeme
    deriving (Show, Read, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1, Ord1) via FunctorClassesDefault (CommentF lexeme)

instance Bifunctor CommentF where
    bimap f g n = case n of
        DocComment as           -> DocComment (map g as)
        DocAttention            -> DocAttention
        DocBrief                -> DocBrief
        DocDeprecated           -> DocDeprecated
        DocExtends l            -> DocExtends (f l)
        DocFile                 -> DocFile
        DocImplements l         -> DocImplements (f l)
        DocNote                 -> DocNote
        DocParam ml l           -> DocParam (fmap f ml) (f l)
        DocReturn               -> DocReturn
        DocRetval               -> DocRetval
        DocSection l            -> DocSection (f l)
        DocSecurityRank l ml l' -> DocSecurityRank (f l) (fmap f ml) (f l')
        DocSee l                -> DocSee (f l)
        DocSubsection l         -> DocSubsection (f l)
        DocPrivate              -> DocPrivate
        DocLine as              -> DocLine (map g as)
        DocCode l as l'         -> DocCode (f l) (map g as) (f l')
        DocWord l               -> DocWord (f l)
        DocRef l                -> DocRef (f l)
        DocP l                  -> DocP (f l)

mapCommentLexeme :: (l -> l') -> Comment l -> Comment l'
mapCommentLexeme f (Fix n) = Fix $ bimap f (mapCommentLexeme f) n

type Comment lexeme = Fix (CommentF lexeme)

instance FromJSON lexeme => FromJSON1 (CommentF lexeme)
instance ToJSON lexeme => ToJSON1 (CommentF lexeme)
instance Hashable lexeme => Hashable1 (CommentF lexeme)

data AssignOp
    = AopEq
    | AopMul
    | AopDiv
    | AopPlus
    | AopMinus
    | AopBitAnd
    | AopBitOr
    | AopBitXor
    | AopMod
    | AopLsh
    | AopRsh
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON AssignOp
instance ToJSON AssignOp

data BinaryOp
    = BopNe
    | BopEq
    | BopOr
    | BopBitXor
    | BopBitOr
    | BopAnd
    | BopBitAnd
    | BopDiv
    | BopMul
    | BopMod
    | BopPlus
    | BopMinus
    | BopLt
    | BopLe
    | BopLsh
    | BopGt
    | BopGe
    | BopRsh
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON BinaryOp
instance ToJSON BinaryOp

data UnaryOp
    = UopNot
    | UopNeg
    | UopMinus
    | UopAddress
    | UopDeref
    | UopIncr
    | UopDecr
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON UnaryOp
instance ToJSON UnaryOp

data LiteralType
    = Char
    | Int
    | Bool
    | String
    | ConstId
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON LiteralType
instance ToJSON LiteralType

data Scope
    = Global
    | Static
    | Local
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON Scope
instance ToJSON Scope

data CommentStyle
    = Regular
    | Doxygen
    | Section
    | Block
    | Ignore
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON CommentStyle
instance ToJSON CommentStyle

data Nullability
    = NullabilityUnspecified
    | Nullable
    | Nonnull
    deriving (Enum, Bounded, Ord, Show, Read, Eq, Generic)

instance FromJSON Nullability
instance ToJSON Nullability

instance (Hashable lexeme, Hashable a) => Hashable (NodeF lexeme a) where
instance (Hashable lexeme, Hashable a) => Hashable (CommentF lexeme a) where
instance Hashable AssignOp where
instance Hashable BinaryOp where
instance Hashable UnaryOp where
instance Hashable LiteralType where
instance Hashable Scope where
instance Hashable CommentStyle where
instance Hashable Nullability where

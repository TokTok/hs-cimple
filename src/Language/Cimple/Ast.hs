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
    ) where

import           Data.Aeson                   (FromJSON, FromJSON1, ToJSON,
                                               ToJSON1)
import           Data.Fix                     (Fix)
import           Data.Functor.Classes         (Eq1, Ord1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           GHC.Generics                 (Generic, Generic1)

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

type Node lexeme = Fix (NodeF lexeme)

instance FromJSON lexeme => FromJSON1 (NodeF lexeme)
instance ToJSON lexeme => ToJSON1 (NodeF lexeme)

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
    | DocSecurityRank lexeme lexeme
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

type Comment lexeme = Fix (CommentF lexeme)

instance FromJSON lexeme => FromJSON1 (CommentF lexeme)
instance ToJSON lexeme => ToJSON1 (CommentF lexeme)

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

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}
module Language.Cimple.AST
    ( AssignOp (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , LiteralType (..)
    , Node (..)
    , Scope (..)
    , CommentStyle (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data Node attr lexeme
    = Attr attr (Node attr lexeme)
    -- Preprocessor
    | PreprocInclude lexeme
    | PreprocDefine lexeme
    | PreprocDefineConst lexeme (Node attr lexeme)
    | PreprocDefineMacro lexeme [Node attr lexeme] (Node attr lexeme)
    | PreprocIf (Node attr lexeme) [Node attr lexeme] (Node attr lexeme)
    | PreprocIfdef lexeme [Node attr lexeme] (Node attr lexeme)
    | PreprocIfndef lexeme [Node attr lexeme] (Node attr lexeme)
    | PreprocElse [Node attr lexeme]
    | PreprocElif (Node attr lexeme) [Node attr lexeme] (Node attr lexeme)
    | PreprocUndef lexeme
    | PreprocDefined lexeme
    | PreprocScopedDefine (Node attr lexeme) [Node attr lexeme] (Node attr lexeme)
    | MacroBodyStmt (Node attr lexeme)
    | MacroBodyFunCall (Node attr lexeme)
    | MacroParam lexeme
    | StaticAssert (Node attr lexeme) lexeme
    -- Comments
    | LicenseDecl lexeme [Node attr lexeme]
    | CopyrightDecl lexeme (Maybe lexeme) [lexeme]
    | Comment CommentStyle lexeme [lexeme] lexeme
    | CommentBlock lexeme
    | Commented (Node attr lexeme) (Node attr lexeme)
    -- Namespace-like blocks
    | ExternC [Node attr lexeme]
    | Class Scope lexeme [Node attr lexeme] [Node attr lexeme]
    | Namespace Scope lexeme [Node attr lexeme]
    -- Statements
    | CompoundStmt [Node attr lexeme]
    | Break
    | Goto lexeme
    | Continue
    | Return (Maybe (Node attr lexeme))
    | SwitchStmt (Node attr lexeme) [Node attr lexeme]
    | IfStmt (Node attr lexeme) (Node attr lexeme) (Maybe (Node attr lexeme))
    | ForStmt (Node attr lexeme) (Node attr lexeme) (Node attr lexeme) (Node attr lexeme)
    | WhileStmt (Node attr lexeme) (Node attr lexeme)
    | DoWhileStmt (Node attr lexeme) (Node attr lexeme)
    | Case (Node attr lexeme) (Node attr lexeme)
    | Default (Node attr lexeme)
    | Label lexeme (Node attr lexeme)
    -- Variable declarations
    | VLA (Node attr lexeme) lexeme (Node attr lexeme)
    | VarDecl (Node attr lexeme) (Node attr lexeme)
    | Declarator (Node attr lexeme) (Maybe (Node attr lexeme))
    | DeclSpecVar lexeme
    | DeclSpecArray (Node attr lexeme) (Maybe (Node attr lexeme))
    -- Expressions
    | InitialiserList [Node attr lexeme]
    | UnaryExpr UnaryOp (Node attr lexeme)
    | BinaryExpr (Node attr lexeme) BinaryOp (Node attr lexeme)
    | TernaryExpr (Node attr lexeme) (Node attr lexeme) (Node attr lexeme)
    | AssignExpr (Node attr lexeme) AssignOp (Node attr lexeme)
    | ParenExpr (Node attr lexeme)
    | CastExpr (Node attr lexeme) (Node attr lexeme)
    | CompoundExpr (Node attr lexeme) (Node attr lexeme)
    | SizeofExpr (Node attr lexeme)
    | SizeofType (Node attr lexeme)
    | LiteralExpr LiteralType lexeme
    | VarExpr lexeme
    | MemberAccess (Node attr lexeme) lexeme
    | PointerAccess (Node attr lexeme) lexeme
    | ArrayAccess (Node attr lexeme) (Node attr lexeme)
    | FunctionCall (Node attr lexeme) [Node attr lexeme]
    | CommentExpr (Node attr lexeme) (Node attr lexeme)
    -- Type definitions
    | EnumClass lexeme [Node attr lexeme]
    | EnumConsts (Maybe lexeme) [Node attr lexeme]
    | EnumDecl lexeme [Node attr lexeme] lexeme
    | Enumerator lexeme (Maybe (Node attr lexeme))
    | ClassForward lexeme [Node attr lexeme]
    | Typedef (Node attr lexeme) lexeme
    | TypedefFunction (Node attr lexeme)
    | Struct lexeme [Node attr lexeme]
    | Union lexeme [Node attr lexeme]
    | MemberDecl (Node attr lexeme) (Node attr lexeme) (Maybe lexeme)
    | TyConst (Node attr lexeme)
    | TyPointer (Node attr lexeme)
    | TyStruct lexeme
    | TyFunc lexeme
    | TyStd lexeme
    | TyVar lexeme
    | TyUserDefined lexeme
    -- Functions
    | FunctionDecl Scope (Node attr lexeme) (Maybe (Node attr lexeme))
    | FunctionDefn Scope (Node attr lexeme) (Node attr lexeme)
    | FunctionPrototype (Node attr lexeme) lexeme [Node attr lexeme]
    | FunctionParam (Node attr lexeme) (Node attr lexeme)
    | Event lexeme (Node attr lexeme)
    | EventParams [Node attr lexeme]
    | Property (Node attr lexeme) (Node attr lexeme) [Node attr lexeme]
    | Accessor lexeme [Node attr lexeme] (Maybe (Node attr lexeme))
    | ErrorDecl lexeme [Node attr lexeme]
    | ErrorList [Node attr lexeme]
    | ErrorFor lexeme
    | Ellipsis
    -- Constants
    | ConstDecl (Node attr lexeme) lexeme
    | ConstDefn Scope (Node attr lexeme) lexeme (Node attr lexeme)
    deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (FromJSON attr, FromJSON lexeme) => FromJSON (Node attr lexeme)
instance (ToJSON attr, ToJSON lexeme) => ToJSON (Node attr lexeme)

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
    deriving (Show, Eq, Generic)

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
    deriving (Show, Eq, Generic)

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
    deriving (Show, Eq, Generic)

instance FromJSON UnaryOp
instance ToJSON UnaryOp

data LiteralType
    = Char
    | Int
    | Bool
    | String
    | ConstId
    deriving (Show, Eq, Generic)

instance FromJSON LiteralType
instance ToJSON LiteralType

data Scope
    = Global
    | Static
    deriving (Show, Eq, Generic)

instance FromJSON Scope
instance ToJSON Scope

data CommentStyle
    = Regular
    | Doxygen
    | Block
    deriving (Show, Eq, Generic)

instance FromJSON CommentStyle
instance ToJSON CommentStyle

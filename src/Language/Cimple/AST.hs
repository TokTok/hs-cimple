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

data Node a lexeme
    = Attr a
    -- Preprocessor
    | PreprocInclude lexeme
    | PreprocDefine lexeme
    | PreprocDefineConst lexeme (Node a lexeme)
    | PreprocDefineMacro lexeme [Node a lexeme] (Node a lexeme)
    | PreprocIf (Node a lexeme) [Node a lexeme] (Node a lexeme)
    | PreprocIfdef lexeme [Node a lexeme] (Node a lexeme)
    | PreprocIfndef lexeme [Node a lexeme] (Node a lexeme)
    | PreprocElse [Node a lexeme]
    | PreprocElif (Node a lexeme) [Node a lexeme] (Node a lexeme)
    | PreprocUndef lexeme
    | PreprocDefined lexeme
    | PreprocScopedDefine (Node a lexeme) [Node a lexeme] (Node a lexeme)
    | MacroBodyStmt [Node a lexeme]
    | MacroBodyFunCall (Node a lexeme)
    | MacroParam lexeme
    | StaticAssert (Node a lexeme) lexeme
    -- Comments
    | LicenseDecl lexeme [Node a lexeme]
    | CopyrightDecl lexeme (Maybe lexeme) [lexeme]
    | Comment CommentStyle lexeme [Node a lexeme] lexeme
    | CommentBlock lexeme
    | CommentWord lexeme
    | Commented (Node a lexeme) (Node a lexeme)
    -- Namespace-like blocks
    | ExternC [Node a lexeme]
    | Class Scope lexeme [Node a lexeme] [Node a lexeme]
    | Namespace Scope lexeme [Node a lexeme]
    -- Statements
    | CompoundStmt [Node a lexeme]
    | Break
    | Goto lexeme
    | Continue
    | Return (Maybe (Node a lexeme))
    | SwitchStmt (Node a lexeme) [Node a lexeme]
    | IfStmt (Node a lexeme) [Node a lexeme] (Maybe (Node a lexeme))
    | ForStmt (Node a lexeme) (Node a lexeme) (Node a lexeme) [Node a lexeme]
    | WhileStmt (Node a lexeme) [Node a lexeme]
    | DoWhileStmt [Node a lexeme] (Node a lexeme)
    | Case (Node a lexeme) (Node a lexeme)
    | Default (Node a lexeme)
    | Label lexeme (Node a lexeme)
    -- Variable declarations
    | VLA (Node a lexeme) lexeme (Node a lexeme)
    | VarDecl (Node a lexeme) (Node a lexeme)
    | Declarator (Node a lexeme) (Maybe (Node a lexeme))
    | DeclSpecVar lexeme
    | DeclSpecArray (Node a lexeme) (Maybe (Node a lexeme))
    -- Expressions
    | InitialiserList [Node a lexeme]
    | UnaryExpr UnaryOp (Node a lexeme)
    | BinaryExpr (Node a lexeme) BinaryOp (Node a lexeme)
    | TernaryExpr (Node a lexeme) (Node a lexeme) (Node a lexeme)
    | AssignExpr (Node a lexeme) AssignOp (Node a lexeme)
    | ParenExpr (Node a lexeme)
    | CastExpr (Node a lexeme) (Node a lexeme)
    | CompoundExpr (Node a lexeme) (Node a lexeme)
    | SizeofExpr (Node a lexeme)
    | SizeofType (Node a lexeme)
    | LiteralExpr LiteralType lexeme
    | VarExpr lexeme
    | MemberAccess (Node a lexeme) lexeme
    | PointerAccess (Node a lexeme) lexeme
    | ArrayAccess (Node a lexeme) (Node a lexeme)
    | FunctionCall (Node a lexeme) [Node a lexeme]
    | CommentExpr (Node a lexeme) (Node a lexeme)
    -- Type definitions
    | EnumClass lexeme [Node a lexeme]
    | EnumConsts (Maybe lexeme) [Node a lexeme]
    | EnumDecl lexeme [Node a lexeme] lexeme
    | Enumerator lexeme (Maybe (Node a lexeme))
    | ClassForward lexeme [Node a lexeme]
    | Typedef (Node a lexeme) lexeme
    | TypedefFunction (Node a lexeme)
    | Struct lexeme [Node a lexeme]
    | Union lexeme [Node a lexeme]
    | MemberDecl (Node a lexeme) (Node a lexeme) (Maybe lexeme)
    | TyConst (Node a lexeme)
    | TyPointer (Node a lexeme)
    | TyStruct lexeme
    | TyFunc lexeme
    | TyStd lexeme
    | TyVar lexeme
    | TyUserDefined lexeme
    -- Functions
    | FunctionDecl Scope (Node a lexeme) (Maybe (Node a lexeme))
    | FunctionDefn Scope (Node a lexeme) [Node a lexeme]
    | FunctionPrototype (Node a lexeme) lexeme [Node a lexeme]
    | FunctionParam (Node a lexeme) (Node a lexeme)
    | Event lexeme (Node a lexeme)
    | EventParams [Node a lexeme]
    | Property (Node a lexeme) (Node a lexeme) [Node a lexeme]
    | Accessor lexeme [Node a lexeme] (Maybe (Node a lexeme))
    | ErrorDecl lexeme [Node a lexeme]
    | ErrorList [Node a lexeme]
    | ErrorFor lexeme
    | Ellipsis
    -- Constants
    | ConstDecl (Node a lexeme) lexeme
    | ConstDefn Scope (Node a lexeme) lexeme (Node a lexeme)
    deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (FromJSON a, FromJSON lexeme) => FromJSON (Node a lexeme)
instance (ToJSON a, ToJSON lexeme) => ToJSON (Node a lexeme)

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

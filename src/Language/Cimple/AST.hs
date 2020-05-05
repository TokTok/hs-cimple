{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
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

data Node lexeme
    -- Preprocessor
    = PreprocInclude lexeme
    | PreprocDefine lexeme
    | PreprocDefineConst lexeme (Node lexeme)
    | PreprocDefineMacro lexeme [Node lexeme] (Node lexeme)
    | PreprocIf (Node lexeme) [Node lexeme] (Node lexeme)
    | PreprocIfdef lexeme [Node lexeme] (Node lexeme)
    | PreprocIfndef lexeme [Node lexeme] (Node lexeme)
    | PreprocElse [Node lexeme]
    | PreprocElif (Node lexeme) [Node lexeme] (Node lexeme)
    | PreprocError lexeme
    | PreprocUndef lexeme
    | PreprocDefined lexeme
    | PreprocScopedDefine (Node lexeme) [Node lexeme] (Node lexeme)
    | MacroBodyStmt [Node lexeme]
    | MacroBodyFunCall (Node lexeme)
    | MacroParam lexeme
    -- Comments
    | Comment CommentStyle [Node lexeme]
    | CommentBlock lexeme
    | CommentWord lexeme
    | Commented (Node lexeme) (Node lexeme)
    -- Namespace-like blocks
    | ExternC [Node lexeme]
    | Class Scope lexeme [Node lexeme] [Node lexeme]
    | Namespace Scope lexeme [Node lexeme]
    -- Statements
    | CompoundStmt [Node lexeme]
    | Break
    | Goto lexeme
    | Continue
    | Return (Maybe (Node lexeme))
    | SwitchStmt (Node lexeme) [Node lexeme]
    | IfStmt (Node lexeme) [Node lexeme] (Maybe (Node lexeme))
    | ForStmt (Maybe (Node lexeme)) (Maybe (Node lexeme)) (Maybe (Node lexeme)) [Node lexeme]
    | WhileStmt (Node lexeme) [Node lexeme]
    | DoWhileStmt [Node lexeme] (Node lexeme)
    | Case (Node lexeme) (Node lexeme)
    | Default (Node lexeme)
    | Label lexeme (Node lexeme)
    -- Variable declarations
    | VLA (Node lexeme) lexeme (Node lexeme)
    | VarDecl (Node lexeme) (Node lexeme)
    | Declarator (Node lexeme) (Maybe (Node lexeme))
    | DeclSpecVar lexeme
    | DeclSpecArray (Node lexeme) (Maybe (Node lexeme))
    -- Expressions
    | InitialiserList [Node lexeme]
    | UnaryExpr UnaryOp (Node lexeme)
    | BinaryExpr (Node lexeme) BinaryOp (Node lexeme)
    | TernaryExpr (Node lexeme) (Node lexeme) (Node lexeme)
    | AssignExpr (Node lexeme) AssignOp (Node lexeme)
    | ParenExpr (Node lexeme)
    | CastExpr (Node lexeme) (Node lexeme)
    | SizeofExpr (Node lexeme)
    | SizeofType (Node lexeme)
    | LiteralExpr LiteralType lexeme
    | VarExpr lexeme
    | MemberAccess (Node lexeme) lexeme
    | PointerAccess (Node lexeme) lexeme
    | ArrayAccess (Node lexeme) (Node lexeme)
    | FunctionCall (Node lexeme) [Node lexeme]
    | CommentExpr (Node lexeme) (Node lexeme)
    -- Type definitions
    | EnumClass lexeme [Node lexeme]
    | EnumConsts (Maybe lexeme) [Node lexeme]
    | EnumDecl lexeme [Node lexeme] lexeme
    | Enumerator lexeme (Maybe (Node lexeme))
    | ClassForward lexeme [Node lexeme]
    | Typedef (Node lexeme) lexeme
    | TypedefFunction (Node lexeme)
    | Struct lexeme [Node lexeme]
    | Union lexeme [Node lexeme]
    | MemberDecl (Node lexeme) (Node lexeme) (Maybe lexeme)
    | TyConst (Node lexeme)
    | TyPointer (Node lexeme)
    | TyStruct lexeme
    | TyFunc lexeme
    | TyStd lexeme
    | TyVar lexeme
    | TyUserDefined lexeme
    -- Functions
    | FunctionDecl Scope (Node lexeme) (Maybe (Node lexeme))
    | FunctionDefn Scope (Node lexeme) [Node lexeme]
    | FunctionPrototype (Node lexeme) lexeme [Node lexeme]
    | FunctionParam (Node lexeme) (Node lexeme)
    | Event lexeme (Node lexeme)
    | EventParams [Node lexeme]
    | Property (Node lexeme) (Node lexeme) [Node lexeme]
    | Accessor lexeme [Node lexeme] (Maybe (Node lexeme))
    | ErrorDecl lexeme [Node lexeme]
    | ErrorList [Node lexeme]
    | ErrorFor lexeme
    | Ellipsis
    -- Constants
    | ConstDecl (Node lexeme) lexeme
    | ConstDefn Scope (Node lexeme) lexeme (Node lexeme)
    deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance FromJSON lexeme => FromJSON (Node lexeme)
instance ToJSON lexeme => ToJSON (Node lexeme)

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

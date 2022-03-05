{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Cimple.TraverseAst
    ( traverseAst

    , doFiles, doFile
    , doNodes, doNode
    , doComment, doComments
    , doLexemes, doLexeme
    , doText

    , astActions, AstActions
    ) where

import           Data.Fix              (Fix (..))
import           Data.Foldable         (traverse_)
import           Language.Cimple.Ast   (Comment, CommentF (..), Node,
                                        NodeF (..))
import           Language.Cimple.Lexer (Lexeme (..))

{-# ANN module "HLint: ignore Reduce duplication" #-}

class TraverseAst text a where
    traverseFileAst
        :: Applicative f
        => AstActions f text
        -> FilePath
        -> a
        -> f ()

traverseAst
    :: (TraverseAst text    a, Applicative f)
    => AstActions f text -> a
    -> f ()
traverseAst = flip traverseFileAst "<stdin>"

data AstActions f text = AstActions
    { doFiles    :: [(FilePath, [Node    (Lexeme text)])] -> f () -> f ()
    , doFile     ::  (FilePath, [Node    (Lexeme text)])  -> f () -> f ()
    , doNodes    :: FilePath -> [Node    (Lexeme text)]   -> f () -> f ()
    , doNode     :: FilePath ->  Node    (Lexeme text)    -> f () -> f ()
    , doComment  :: FilePath ->  Comment (Lexeme text)    -> f () -> f ()
    , doComments :: FilePath -> [Comment (Lexeme text)]   -> f () -> f ()
    , doLexemes  :: FilePath ->          [Lexeme text]    -> f () -> f ()
    , doLexeme   :: FilePath ->           Lexeme text     -> f () -> f ()
    , doText     :: FilePath ->                  text             -> f ()
    }

instance TraverseAst text        a
      => TraverseAst text (Maybe a) where
    traverseFileAst _ _ _ = pure ()

astActions
    :: Applicative f
    => AstActions f text
astActions = AstActions
    { doFiles     = const id
    , doFile      = const id
    , doNodes     = const $ const id
    , doNode      = const $ const id
    , doComment   = const $ const id
    , doComments  = const $ const id
    , doLexeme    = const $ const id
    , doLexemes   = const $ const id
    , doText      = const $ const $ pure ()
    }


instance TraverseAst text (Lexeme text) where
    traverseFileAst :: forall f . Applicative f
               => AstActions f text -> FilePath -> Lexeme text -> f ()
    traverseFileAst AstActions{..} currentFile = doLexeme currentFile <*>
        \(L _ _ s) -> doText currentFile s

instance TraverseAst text [Lexeme text] where
    traverseFileAst actions@AstActions{..} currentFile = doLexemes currentFile <*>
        traverse_ (traverseFileAst actions currentFile)

instance TraverseAst text (Comment (Lexeme text)) where
    traverseFileAst
        :: forall f . Applicative f
        => AstActions f text
        -> FilePath
        -> Comment (Lexeme text)
        -> f ()
    traverseFileAst actions@AstActions{..} currentFile = doComment currentFile <*> \comment -> case unFix comment of
        DocComment docs ->
            recurse docs
        DocWord word ->
            recurse word
        DocSentence docs ending -> do
            _ <- recurse docs
            _ <- recurse ending
            pure ()
        DocNewline -> pure ()

        DocAttention docs ->
            recurse docs
        DocBrief docs ->
            recurse docs
        DocDeprecated docs ->
            recurse docs
        DocParam attr name docs -> do
            _ <- recurse attr
            _ <- recurse name
            _ <- recurse docs
            pure ()
        DocReturn docs ->
            recurse docs
        DocRetval expr docs -> do
            _ <- recurse expr
            _ <- recurse docs
            pure ()
        DocSee ref docs -> do
            _ <- recurse ref
            _ <- recurse docs
            pure ()

        DocLine docs ->
            recurse docs
        DocBullet docs sublist -> do
            _ <- recurse docs
            _ <- recurse sublist
            pure ()
        DocBulletList docs ->
            recurse docs

        DocColon docs ->
            recurse docs
        DocRef doc ->
            recurse doc
        DocP doc ->
            recurse doc
        DocLParen docs ->
            recurse docs
        DocRParen docs ->
            recurse docs
        DocAssignOp _ lhs rhs -> do
            _ <- recurse lhs
            _ <- recurse rhs
            pure ()
        DocBinaryOp _ lhs rhs -> do
            _ <- recurse lhs
            _ <- recurse rhs
            pure ()

      where
        recurse :: TraverseAst text a => a -> f ()
        recurse = traverseFileAst actions currentFile

instance TraverseAst text [Comment (Lexeme text)] where
    traverseFileAst actions@AstActions{..} currentFile = doComments currentFile <*>
        traverse_ (traverseFileAst actions currentFile)

instance TraverseAst text (Node (Lexeme text)) where
    traverseFileAst
        :: forall f . Applicative f
        => AstActions f text
        -> FilePath
        ->    Node (Lexeme text)
        -> f ()
    traverseFileAst actions@AstActions{..} currentFile = doNode currentFile <*> \node -> case unFix node of
        PreprocInclude path ->
            recurse path
        PreprocDefine name ->
            recurse name
        PreprocDefineConst name value -> do
            _ <- recurse name
            _ <- recurse value
            pure ()
        PreprocDefineMacro name params body -> do
            _ <- recurse name
            _ <- recurse params
            _ <- recurse body
            pure ()
        PreprocIf cond thenDecls elseBranch -> do
            _ <- recurse cond
            _ <- recurse thenDecls
            _ <- recurse elseBranch
            pure ()
        PreprocIfdef name thenDecls elseBranch -> do
            _ <- recurse name
            _ <- recurse thenDecls
            _ <- recurse elseBranch
            pure ()
        PreprocIfndef name thenDecls elseBranch -> do
            _ <- recurse name
            _ <- recurse thenDecls
            _ <- recurse elseBranch
            pure ()
        PreprocElse decls ->
            recurse decls
        PreprocElif cond decls elseBranch -> do
            _ <- recurse cond
            _ <- recurse decls
            _ <- recurse elseBranch
            pure ()
        PreprocUndef name ->
            recurse name
        PreprocDefined name ->
            recurse name
        PreprocScopedDefine define stmts undef -> do
            _ <- recurse define
            _ <- recurse stmts
            _ <- recurse undef
            pure ()
        MacroBodyStmt stmts ->
            recurse stmts
        MacroBodyFunCall expr ->
            recurse expr
        MacroParam name ->
            recurse name
        StaticAssert cond msg -> do
            _ <- recurse cond
            _ <- recurse msg
            pure ()
        LicenseDecl license copyrights -> do
            _ <- recurse license
            _ <- recurse copyrights
            pure ()
        CopyrightDecl from to owner -> do
            _ <- recurse from
            _ <- recurse to
            _ <- recurse owner
            pure ()
        Comment _doc start contents end -> do
            _ <- recurse start
            _ <- recurse contents
            _ <- recurse end
            pure ()
        CommentSection start decls end -> do
            _ <- recurse start
            _ <- recurse decls
            _ <- recurse end
            pure ()
        CommentSectionEnd comment -> do
            _ <- recurse comment
            pure ()
        Commented comment subject -> do
            _ <- recurse comment
            _ <- recurse subject
            pure ()
        CommentInfo comment ->
            recurse comment
        ExternC decls ->
            recurse decls
        Group decls ->
            recurse decls
        CompoundStmt stmts ->
            recurse stmts
        Break ->
            pure ()
        Goto label ->
            recurse label
        Continue ->
            pure ()
        Return value ->
            recurse value
        SwitchStmt value cases -> do
            _ <- recurse value
            _ <- recurse cases
            pure ()
        IfStmt cond thenStmts elseStmt -> do
            _ <- recurse cond
            _ <- recurse thenStmts
            _ <- recurse elseStmt
            pure ()
        ForStmt initStmt cond next stmts -> do
            _ <- recurse initStmt
            _ <- recurse cond
            _ <- recurse next
            _ <- recurse stmts
            pure ()
        WhileStmt cond stmts -> do
            _ <- recurse cond
            _ <- recurse stmts
            pure ()
        DoWhileStmt stmts cond -> do
            _ <- recurse stmts
            _ <- recurse cond
            pure ()
        Case value stmt -> do
            _ <- recurse value
            _ <- recurse stmt
            pure ()
        Default stmt ->
            recurse stmt
        Label label stmt -> do
            _ <- recurse label
            _ <- recurse stmt
            pure ()
        ExprStmt expr -> do
            _ <- recurse expr
            pure ()
        VLA ty name size -> do
            _ <- recurse ty
            _ <- recurse name
            _ <- recurse size
            pure ()
        VarDeclStmt decl ini -> do
            _ <- recurse decl
            _ <- recurse ini
            pure ()
        VarDecl ty name arrs -> do
            _ <- recurse ty
            _ <- recurse name
            _ <- recurse arrs
            pure ()
        DeclSpecArray size ->
            recurse size
        InitialiserList values ->
            recurse values
        UnaryExpr _op expr ->
            recurse expr
        BinaryExpr lhs _op rhs -> do
            _ <- recurse lhs
            _ <- recurse rhs
            pure ()
        TernaryExpr cond thenExpr elseExpr -> do
            _ <- recurse cond
            _ <- recurse thenExpr
            _ <- recurse elseExpr
            pure ()
        AssignExpr lhs _op rhs -> do
            _ <- recurse lhs
            _ <- recurse rhs
            pure ()
        ParenExpr expr ->
            recurse expr
        CastExpr ty expr -> do
            _ <- recurse ty
            _ <- recurse expr
            pure ()
        CompoundExpr ty expr -> do -- DEPRECATED
            _ <- recurse ty
            _ <- recurse expr
            pure ()
        CompoundLiteral ty expr -> do
            _ <- recurse ty
            _ <- recurse expr
            pure ()
        SizeofExpr expr ->
            recurse expr
        SizeofType ty ->
            recurse ty
        LiteralExpr _ty value ->
            recurse value
        VarExpr name ->
            recurse name
        MemberAccess name field -> do
            _ <- recurse name
            _ <- recurse field
            pure ()
        PointerAccess name field -> do
            _ <- recurse name
            _ <- recurse field
            pure ()
        ArrayAccess arr idx -> do
            _ <- recurse arr
            _ <- recurse idx
            pure ()
        FunctionCall callee args -> do
            _ <- recurse callee
            _ <- recurse args
            pure ()
        CommentExpr comment expr -> do
            _ <- recurse comment
            _ <- recurse expr
            pure ()
        EnumConsts name members -> do
            _ <- recurse name
            _ <- recurse members
            pure ()
        EnumDecl name members tyName -> do
            _ <- recurse name
            _ <- recurse members
            _ <- recurse tyName
            pure ()
        Enumerator name value -> do
            _ <- recurse name
            _ <- recurse value
            pure ()
        AggregateDecl struct -> do
            _ <- recurse struct
            pure ()
        Typedef ty name -> do
            _ <- recurse ty
            _ <- recurse name
            pure ()
        TypedefFunction ty ->
            recurse ty
        Struct name members -> do
            _ <- recurse name
            _ <- recurse members
            pure ()
        Union name members -> do
            _ <- recurse name
            _ <- recurse members
            pure ()
        MemberDecl decl bits -> do
            _ <- recurse decl
            _ <- recurse bits
            pure ()
        TyConst ty ->
            recurse ty
        TyPointer ty ->
            recurse ty
        TyStruct name ->
            recurse name
        TyFunc name ->
            recurse name
        TyStd name ->
            recurse name
        TyUserDefined name ->
            recurse name
        AttrPrintf fmt ellipsis fun -> do
            _ <- recurse fmt
            _ <- recurse ellipsis
            _ <- recurse fun
            pure ()
        FunctionDecl _scope proto ->
            recurse proto
        FunctionDefn _scope proto body -> do
            _ <- recurse proto
            _ <- recurse body
            pure ()
        FunctionPrototype ty name params -> do
            _ <- recurse ty
            _ <- recurse name
            _ <- recurse params
            pure ()
        CallbackDecl ty name -> do
            _ <- recurse ty
            _ <- recurse name
            pure ()
        Ellipsis ->
            pure ()
        NonNull nonnull nullable f -> do
            _ <- recurse nonnull
            _ <- recurse nullable
            _ <- recurse f
            pure ()
        ConstDecl ty name -> do
            _ <- recurse ty
            _ <- recurse name
            pure ()
        ConstDefn _scope ty name value -> do
            _ <- recurse ty
            _ <- recurse name
            _ <- recurse value
            pure ()

      where
        recurse :: TraverseAst text a => a -> f ()
        recurse = traverseFileAst actions currentFile

instance TraverseAst text [Node (Lexeme text)] where
    traverseFileAst actions@AstActions{..} currentFile = doNodes currentFile <*>
        traverse_ (traverseFileAst actions currentFile)

instance TraverseAst text (FilePath, [Node (Lexeme text)]) where
    traverseFileAst actions@AstActions{..} _ tu@(currentFile, _) = doFile <*>
        traverse_ (traverseFileAst actions currentFile) $ tu

instance TraverseAst text [(FilePath, [Node (Lexeme text)])] where
    traverseFileAst actions@AstActions{..} currentFile = doFiles <*>
        traverse_ (traverseFileAst actions currentFile)

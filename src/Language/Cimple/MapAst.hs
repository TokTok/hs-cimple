{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Cimple.MapAst
    ( mapAst, mapFileAst

    , doFiles, doFile
    , doNodes, doNode
    , doComment, doComments
    , doLexemes, doLexeme
    , doText

    , AstActions, astActions
    , TextActions, textActions
    , IdentityActions, identityActions
    ) where

import           Data.Fix              (Fix (..))
import           GHC.Stack             (HasCallStack)
import           Language.Cimple.Ast   (Comment, CommentF (..), Node,
                                        NodeF (..))
import           Language.Cimple.Lexer (Lexeme (..))

class MapAst itext otext a where
    type Mapped itext otext a
    mapFileAst
        :: (Applicative f, HasCallStack)
        => AstActions f itext otext
        -> FilePath
        -> a
        -> f (Mapped itext otext a)

mapAst
    :: (MapAst itext otext a, Applicative f, HasCallStack)
    => AstActions f itext otext -> a
    -> f    (Mapped itext otext    a)
mapAst = flip mapFileAst "<stdin>"

data AstActions f itext otext = AstActions
    { doFiles     :: [(FilePath, [Node    (Lexeme itext)])] -> f [(FilePath, [Node    (Lexeme otext)])] -> f [(FilePath, [Node    (Lexeme otext)])]
    , doFile      ::  (FilePath, [Node    (Lexeme itext)])  -> f  (FilePath, [Node    (Lexeme otext)])  -> f  (FilePath, [Node    (Lexeme otext)])
    , doNodes     :: FilePath -> [Node    (Lexeme itext)]   -> f             [Node    (Lexeme otext)]   -> f             [Node    (Lexeme otext)]
    , doNode      :: FilePath ->  Node    (Lexeme itext)    -> f             (Node    (Lexeme otext))   -> f             (Node    (Lexeme otext))
    , doComment   :: FilePath ->  Comment (Lexeme itext)    -> f             (Comment (Lexeme otext))   -> f             (Comment (Lexeme otext))
    , doComments  :: FilePath -> [Comment (Lexeme itext)]   -> f             [Comment (Lexeme otext)]   -> f             [Comment (Lexeme otext)]
    , doLexemes   :: FilePath ->          [Lexeme itext]    -> f                      [Lexeme otext]    -> f                      [Lexeme otext]
    , doLexeme    :: FilePath ->           Lexeme itext     -> f                      (Lexeme otext)    -> f                      (Lexeme otext)
    , doText      :: FilePath ->                  itext                                                 -> f                              otext
    }

instance MapAst itext otext        a
      => MapAst itext otext (Maybe a) where
    type        Mapped itext otext (Maybe a)
       = Maybe (Mapped itext otext        a)
    mapFileAst _       _           Nothing  = pure Nothing
    mapFileAst actions currentFile (Just x) = Just <$> mapFileAst actions currentFile x

astActions
    :: Applicative f
    => (itext -> f otext)
    -> AstActions f itext otext
astActions ft = AstActions
    { doFiles     = const id
    , doFile      = const id
    , doNodes     = const $ const id
    , doNode      = const $ const id
    , doComment   = const $ const id
    , doComments  = const $ const id
    , doLexeme    = const $ const id
    , doLexemes   = const $ const id
    , doText      = const ft
    }

type TextActions f itext otext = AstActions f itext otext
textActions :: Applicative f => (itext -> f otext) -> TextActions f itext otext
textActions = astActions

type IdentityActions f text = AstActions f text text
identityActions :: Applicative f => AstActions f text text
identityActions = astActions pure


instance MapAst itext otext (Lexeme itext) where
    type Mapped itext otext (Lexeme itext)
                          =  Lexeme otext
    mapFileAst :: (Applicative f, HasCallStack)
               => AstActions f itext otext -> FilePath -> Lexeme itext -> f (Lexeme otext)
    mapFileAst AstActions{..} currentFile = doLexeme currentFile <*>
        \(L p c s) -> L p c <$> doText currentFile s

instance MapAst itext otext [Lexeme itext] where
    type Mapped itext otext [Lexeme itext]
                          = [Lexeme otext]
    mapFileAst actions@AstActions{..} currentFile = doLexemes currentFile <*>
        traverse (mapFileAst actions currentFile)

instance MapAst itext otext (Comment (Lexeme itext)) where
    type Mapped itext otext (Comment (Lexeme itext))
                          =  Comment (Lexeme otext)
    mapFileAst
        :: forall f. (Applicative f, HasCallStack)
        => AstActions f itext otext
        -> FilePath
        ->    Comment (Lexeme itext)
        -> f (Comment (Lexeme otext))
    mapFileAst actions@AstActions{..} currentFile = doComment currentFile <*> \comment -> case unFix comment of
        DocComment docs ->
            Fix <$> (DocComment <$> recurse docs)
        DocWord word ->
            Fix <$> (DocWord <$> recurse word)
        DocSentence docs ending ->
            Fix <$> (DocSentence <$> recurse docs <*> recurse ending)
        DocNewline -> pure $ Fix DocNewline

        DocAttention docs ->
            Fix <$> (DocAttention <$> recurse docs)
        DocBrief docs ->
            Fix <$> (DocBrief <$> recurse docs)
        DocDeprecated docs ->
            Fix <$> (DocDeprecated <$> recurse docs)
        DocExtends feat ->
            Fix <$> (DocExtends <$> recurse feat)
        DocImplements feat ->
            Fix <$> (DocImplements <$> recurse feat)
        DocParam attr name docs ->
            Fix <$> (DocParam <$> recurse attr <*> recurse name <*> recurse docs)
        DocReturn docs ->
            Fix <$> (DocReturn <$> recurse docs)
        DocRetval expr docs ->
            Fix <$> (DocRetval <$> recurse expr <*> recurse docs)
        DocSee ref docs ->
            Fix <$> (DocSee <$> recurse ref <*> recurse docs)

        DocPrivate -> pure $ Fix DocPrivate

        DocParagraph docs ->
            Fix <$> (DocParagraph <$> recurse docs)
        DocLine docs ->
            Fix <$> (DocLine <$> recurse docs)
        DocCode begin docs end ->
            Fix <$> (DocCode <$> recurse begin <*> recurse docs <*> recurse end)
        DocList docs ->
            Fix <$> (DocList <$> recurse docs)
        DocOLItem docs sublist ->
            Fix <$> (DocOLItem <$> recurse docs <*> recurse sublist)
        DocULItem docs sublist ->
            Fix <$> (DocULItem <$> recurse docs <*> recurse sublist)

        DocColon docs ->
            Fix <$> (DocColon <$> recurse docs)
        DocRef doc ->
            Fix <$> (DocRef <$> recurse doc)
        DocP doc ->
            Fix <$> (DocP <$> recurse doc)
        DocLParen docs ->
            Fix <$> (DocLParen <$> recurse docs)
        DocRParen docs ->
            Fix <$> (DocRParen <$> recurse docs)
        DocAssignOp op lhs rhs ->
            Fix <$> (DocAssignOp op <$> recurse lhs <*> recurse rhs)
        DocBinaryOp op lhs rhs ->
            Fix <$> (DocBinaryOp op <$> recurse lhs <*> recurse rhs)

      where
        recurse :: MapAst itext otext a => a -> f (Mapped itext otext a)
        recurse = mapFileAst actions currentFile

instance MapAst itext otext [Comment (Lexeme itext)] where
    type Mapped itext otext [Comment (Lexeme itext)]
                          = [Comment (Lexeme otext)]
    mapFileAst actions@AstActions{..} currentFile = doComments currentFile <*>
        traverse (mapFileAst actions currentFile)


instance MapAst itext otext (Node (Lexeme itext)) where
    type Mapped itext otext (Node (Lexeme itext))
                          =  Node (Lexeme otext)
    mapFileAst
        :: forall f . (Applicative f, HasCallStack)
        => AstActions f itext otext
        -> FilePath
        ->    Node (Lexeme itext)
        -> f (Node (Lexeme otext))
    mapFileAst actions@AstActions{..} currentFile = doNode currentFile <*> \node -> case unFix node of
        PreprocInclude path ->
            Fix <$> (PreprocInclude <$> recurse path)
        PreprocDefine name ->
            Fix <$> (PreprocDefine <$> recurse name)
        PreprocDefineConst name value ->
            Fix <$> (PreprocDefineConst <$> recurse name <*> recurse value)
        PreprocDefineMacro name params body ->
            Fix <$> (PreprocDefineMacro <$> recurse name <*> recurse params <*> recurse body)
        PreprocIf cond thenDecls elseBranch ->
            Fix <$> (PreprocIf <$> recurse cond <*> recurse thenDecls <*> recurse elseBranch)
        PreprocIfdef name thenDecls elseBranch ->
            Fix <$> (PreprocIfdef <$> recurse name <*> recurse thenDecls <*> recurse elseBranch)
        PreprocIfndef name thenDecls elseBranch ->
            Fix <$> (PreprocIfndef <$> recurse name <*> recurse thenDecls <*> recurse elseBranch)
        PreprocElse decls ->
            Fix <$> (PreprocElse <$> recurse decls)
        PreprocElif cond decls elseBranch ->
            Fix <$> (PreprocElif <$> recurse cond <*> recurse decls <*> recurse elseBranch)
        PreprocUndef name ->
            Fix <$> (PreprocUndef <$> recurse name)
        PreprocDefined name ->
            Fix <$> (PreprocDefined <$> recurse name)
        PreprocScopedDefine define stmts undef ->
            Fix <$> (PreprocScopedDefine <$> recurse define <*> recurse stmts <*> recurse undef)
        MacroBodyStmt stmts ->
            Fix <$> (MacroBodyStmt <$> recurse stmts)
        MacroBodyFunCall expr ->
            Fix <$> (MacroBodyFunCall <$> recurse expr)
        MacroParam name ->
            Fix <$> (MacroParam <$> recurse name)
        StaticAssert cond msg ->
            Fix <$> (StaticAssert <$> recurse cond <*> recurse msg)
        LicenseDecl license copyrights ->
            Fix <$> (LicenseDecl <$> recurse license <*> recurse copyrights)
        CopyrightDecl from to owner ->
            Fix <$> (CopyrightDecl <$> recurse from <*> recurse to <*> recurse owner)
        Comment doc start contents end ->
            Fix <$> (Comment doc <$> recurse start <*> recurse contents <*> recurse end)
        CommentSection start decls end ->
            Fix <$> (CommentSection <$> recurse start <*> recurse decls <*> recurse end)
        CommentSectionEnd comment ->
            Fix <$> (CommentSectionEnd <$> recurse comment)
        Commented comment subject ->
            Fix <$> (Commented <$> recurse comment <*> recurse subject)
        CommentInfo comment ->
            Fix <$> (CommentInfo <$> recurse comment)
        ExternC decls ->
            Fix <$> (ExternC <$> recurse decls)
        Group decls ->
            Fix <$> (Group <$> recurse decls)
        CompoundStmt stmts ->
            Fix <$> (CompoundStmt <$> recurse stmts)
        Break ->
            pure $ Fix Break
        Goto label ->
            Fix <$> (Goto <$> recurse label)
        Continue ->
            pure $ Fix Continue
        Return value ->
            Fix <$> (Return <$> recurse value)
        SwitchStmt value cases ->
            Fix <$> (SwitchStmt <$> recurse value <*> recurse cases)
        IfStmt cond thenStmts elseStmt ->
            Fix <$> (IfStmt <$> recurse cond <*> recurse thenStmts <*> recurse elseStmt)
        ForStmt initStmt cond next stmts ->
            Fix <$> (ForStmt <$> recurse initStmt <*> recurse cond <*> recurse next <*> recurse stmts)
        WhileStmt cond stmts ->
            Fix <$> (WhileStmt <$> recurse cond <*> recurse stmts)
        DoWhileStmt stmts cond ->
            Fix <$> (DoWhileStmt <$> recurse stmts <*> recurse cond)
        Case value stmt ->
            Fix <$> (Case <$> recurse value <*> recurse stmt)
        Default stmt ->
            Fix <$> (Default <$> recurse stmt)
        Label label stmt ->
            Fix <$> (Label <$> recurse label <*> recurse stmt)
        ExprStmt expr ->
            Fix <$> (ExprStmt <$> recurse expr)
        VLA ty name size ->
            Fix <$> (VLA <$> recurse ty <*> recurse name <*> recurse size)
        VarDeclStmt decl ini ->
            Fix <$> (VarDeclStmt <$> recurse decl <*> recurse ini)
        VarDecl ty name arrs ->
            Fix <$> (VarDecl <$> recurse ty <*> recurse name <*> recurse arrs)
        DeclSpecArray size ->
            Fix <$> (DeclSpecArray <$> recurse size)
        InitialiserList values ->
            Fix <$> (InitialiserList <$> recurse values)
        UnaryExpr op expr ->
            Fix <$> (UnaryExpr op <$> recurse expr)
        BinaryExpr lhs op rhs ->
            Fix <$> (BinaryExpr <$> recurse lhs <*> pure op <*> recurse rhs)
        TernaryExpr cond thenExpr elseExpr ->
            Fix <$> (TernaryExpr <$> recurse cond <*> recurse thenExpr <*> recurse elseExpr)
        AssignExpr lhs op rhs ->
            Fix <$> (AssignExpr <$> recurse lhs <*> pure op <*> recurse rhs)
        ParenExpr expr ->
            Fix <$> (ParenExpr <$> recurse expr)
        CastExpr ty expr ->
            Fix <$> (CastExpr <$> recurse ty <*> recurse expr)
        CompoundExpr ty expr -> -- DEPRECATED
            Fix <$> (CompoundExpr <$> recurse ty <*> recurse expr)
        CompoundLiteral ty expr ->
            Fix <$> (CompoundLiteral <$> recurse ty <*> recurse expr)
        SizeofExpr expr ->
            Fix <$> (SizeofExpr <$> recurse expr)
        SizeofType ty ->
            Fix <$> (SizeofType <$> recurse ty)
        LiteralExpr ty value ->
            Fix <$> (LiteralExpr ty <$> recurse value)
        VarExpr name ->
            Fix <$> (VarExpr <$> recurse name)
        MemberAccess name field ->
            Fix <$> (MemberAccess <$> recurse name <*> recurse field)
        PointerAccess name field ->
            Fix <$> (PointerAccess <$> recurse name <*> recurse field)
        ArrayAccess arr idx ->
            Fix <$> (ArrayAccess <$> recurse arr <*> recurse idx)
        FunctionCall callee args ->
            Fix <$> (FunctionCall <$> recurse callee <*> recurse args)
        CommentExpr comment expr ->
            Fix <$> (CommentExpr <$> recurse comment <*> recurse expr)
        EnumConsts name members ->
            Fix <$> (EnumConsts <$> recurse name <*> recurse members)
        EnumDecl name members tyName ->
            Fix <$> (EnumDecl <$> recurse name <*> recurse members <*> recurse tyName)
        Enumerator name value ->
            Fix <$> (Enumerator <$> recurse name <*> recurse value)
        AggregateDecl struct ->
            Fix <$> (AggregateDecl <$> recurse struct)
        Typedef ty name ->
            Fix <$> (Typedef <$> recurse ty <*> recurse name)
        TypedefFunction ty ->
            Fix <$> (TypedefFunction <$> recurse ty)
        Struct name members ->
            Fix <$> (Struct <$> recurse name <*> recurse members)
        Union name members ->
            Fix <$> (Union <$> recurse name <*> recurse members)
        MemberDecl decl bits ->
            Fix <$> (MemberDecl <$> recurse decl <*> recurse bits)
        TyConst ty ->
            Fix <$> (TyConst <$> recurse ty)
        TyPointer ty ->
            Fix <$> (TyPointer <$> recurse ty)
        TyStruct name ->
            Fix <$> (TyStruct <$> recurse name)
        TyFunc name ->
            Fix <$> (TyFunc <$> recurse name)
        TyStd name ->
            Fix <$> (TyStd <$> recurse name)
        TyUserDefined name ->
            Fix <$> (TyUserDefined <$> recurse name)
        AttrPrintf fmt ellipsis fun ->
            Fix <$> (AttrPrintf <$> recurse fmt <*> recurse ellipsis <*> recurse fun)
        FunctionDecl scope proto ->
            Fix <$> (FunctionDecl scope <$> recurse proto)
        FunctionDefn scope proto body ->
            Fix <$> (FunctionDefn scope <$> recurse proto <*> recurse body)
        FunctionPrototype ty name params ->
            Fix <$> (FunctionPrototype <$> recurse ty <*> recurse name <*> recurse params)
        CallbackDecl ty name ->
            Fix <$> (CallbackDecl <$> recurse ty <*> recurse name)
        NonNull nonnull nullable f ->
            Fix <$> (NonNull <$> recurse nonnull <*> recurse nullable <*> recurse f)
        Ellipsis ->
            pure $ Fix Ellipsis
        ConstDecl ty name ->
            Fix <$> (ConstDecl <$> recurse ty <*> recurse name)
        ConstDefn scope ty name value ->
            Fix <$> (ConstDefn scope <$> recurse ty <*> recurse name <*> recurse value)

      where
        recurse :: MapAst itext otext a => a -> f (Mapped itext otext a)
        recurse = mapFileAst actions currentFile

instance MapAst itext otext [Node (Lexeme itext)] where
    type Mapped itext otext [Node (Lexeme itext)]
                          = [Node (Lexeme otext)]
    mapFileAst actions@AstActions{..} currentFile = doNodes currentFile <*>
        traverse (mapFileAst actions currentFile)

instance MapAst itext otext (FilePath, [Node (Lexeme itext)]) where
    type Mapped itext otext (FilePath, [Node (Lexeme itext)])
                          = (FilePath, [Node (Lexeme otext)])
    mapFileAst actions@AstActions{..} _ tu@(currentFile, _) = doFile <*>
        traverse (mapFileAst actions currentFile) $ tu

instance MapAst itext otext [(FilePath, [Node (Lexeme itext)])] where
    type Mapped itext otext [(FilePath, [Node (Lexeme itext)])]
                          = [(FilePath, [Node (Lexeme otext)])]
    mapFileAst actions@AstActions{..} currentFile = doFiles <*>
        traverse (mapFileAst actions currentFile)

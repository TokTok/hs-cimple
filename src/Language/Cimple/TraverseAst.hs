{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Cimple.TraverseAst
    ( traverseAst

    , doFiles, doFile
    , doNodes, doNode
    , doLexemes, doLexeme
    , doText
    , doAttr

    , astActions
    , AttrActions, attrActions
    , TextActions, textActions
    , IdentityActions, identityActions
    ) where

import           Language.Cimple.AST   (Node (..))
import           Language.Cimple.Lexer (Lexeme (..))

class TraverseAst iattr oattr itext otext a where
    type Mapped iattr oattr itext otext a
    mapFileAst
        :: Applicative f
        => AstActions f iattr oattr itext otext
        -> FilePath
        -> a
        -> f (Mapped iattr oattr itext otext a)

traverseAst
    :: (TraverseAst iattr oattr itext otext    a, Applicative f)
    => AstActions f iattr oattr itext otext -> a
    -> f    (Mapped iattr oattr itext otext    a)
traverseAst = flip mapFileAst "<stdin>"

data AstActions f iattr oattr itext otext = AstActions
    { doFiles     :: [(FilePath, [Node iattr (Lexeme itext)])] -> f [(FilePath, [Node oattr (Lexeme otext)])] -> f [(FilePath, [Node oattr (Lexeme otext)])]
    , doFile      ::  (FilePath, [Node iattr (Lexeme itext)])  -> f  (FilePath, [Node oattr (Lexeme otext)])  -> f  (FilePath, [Node oattr (Lexeme otext)])
    , doNodes     :: FilePath -> [Node iattr (Lexeme itext)]   -> f             [Node oattr (Lexeme otext)]   -> f             [Node oattr (Lexeme otext)]
    , doNode      :: FilePath ->  Node iattr (Lexeme itext)    -> f             (Node oattr (Lexeme otext))   -> f             (Node oattr (Lexeme otext))
    , doLexemes   :: FilePath ->             [Lexeme itext]    -> f                         [Lexeme otext]    -> f                         [Lexeme otext]
    , doLexeme    :: FilePath ->              Lexeme itext     -> f                         (Lexeme otext)    -> f                         (Lexeme otext)
    , doText      :: FilePath ->                     itext                                                    -> f                                 otext
    , doAttr      :: FilePath ->       iattr                                                                  -> f                   oattr
    }

instance TraverseAst iattr oattr itext otext        a
      => TraverseAst iattr oattr itext otext (Maybe a) where
    type        Mapped iattr oattr itext otext (Maybe a)
       = Maybe (Mapped iattr oattr itext otext        a)
    mapFileAst _       _           Nothing  = pure Nothing
    mapFileAst actions currentFile (Just x) = Just <$> mapFileAst actions currentFile x

astActions
    :: Applicative f
    => (iattr -> f oattr)
    -> (itext -> f otext)
    -> AstActions f iattr oattr itext otext
astActions fa ft = AstActions
    { doFiles     = const id
    , doFile      = const id
    , doNodes     = const $ const id
    , doNode      = const $ const id
    , doLexeme    = const $ const id
    , doLexemes   = const $ const id
    , doText      = const ft
    , doAttr      = const fa
    }

type AttrActions f iattr oattr text = AstActions f iattr oattr text text
attrActions :: Applicative f => (iattr -> f oattr) -> AttrActions f iattr oattr text
attrActions = flip astActions pure

type TextActions f attr itext otext = AstActions f attr attr itext otext
textActions :: Applicative f => (itext -> f otext) -> TextActions f attr itext otext
textActions = astActions pure

type IdentityActions f attr text = AstActions f attr attr text text
identityActions :: Applicative f => AstActions f attr attr text text
identityActions = astActions pure pure


instance TraverseAst iattr oattr itext otext (Lexeme itext) where
    type Mapped iattr oattr itext otext (Lexeme itext)
                                      =  Lexeme otext
    mapFileAst :: forall f . Applicative f
               => AstActions f iattr oattr itext otext -> FilePath -> Lexeme itext -> f (Lexeme otext)
    mapFileAst AstActions{..} currentFile = doLexeme currentFile <*>
        \(L p c s) -> L p c <$> doText currentFile s

instance TraverseAst iattr oattr itext otext [Lexeme itext] where
    type Mapped iattr oattr itext otext [Lexeme itext]
                                      = [Lexeme otext]
    mapFileAst actions@AstActions{..} currentFile = doLexemes currentFile <*>
        traverse (mapFileAst actions currentFile)

instance TraverseAst iattr oattr itext otext (Node iattr (Lexeme itext)) where
    type Mapped iattr oattr itext otext (Node iattr (Lexeme itext))
                                      =  Node oattr (Lexeme otext)
    mapFileAst
        :: forall f . Applicative f
        => AstActions f iattr oattr itext otext
        -> FilePath
        ->    Node iattr (Lexeme itext)
        -> f (Node oattr (Lexeme otext))
    mapFileAst actions@AstActions{..} currentFile = doNode currentFile <*> \case
        Attr attr node ->
            Attr <$> doAttr currentFile attr <*> recurse node
        PreprocInclude path ->
            PreprocInclude <$> recurse path
        PreprocDefine name ->
            PreprocDefine <$> recurse name
        PreprocDefineConst name value ->
            PreprocDefineConst <$> recurse name <*> recurse value
        PreprocDefineMacro name params body ->
            PreprocDefineMacro <$> recurse name <*> recurse params <*> recurse body
        PreprocIf cond thenDecls elseBranch ->
            PreprocIf <$> recurse cond <*> recurse thenDecls <*> recurse elseBranch
        PreprocIfdef name thenDecls elseBranch ->
            PreprocIfdef <$> recurse name <*> recurse thenDecls <*> recurse elseBranch
        PreprocIfndef name thenDecls elseBranch ->
            PreprocIfndef <$> recurse name <*> recurse thenDecls <*> recurse elseBranch
        PreprocElse decls ->
            PreprocElse <$> recurse decls
        PreprocElif cond decls elseBranch ->
            PreprocElif <$> recurse cond <*> recurse decls <*> recurse elseBranch
        PreprocUndef name ->
            PreprocUndef <$> recurse name
        PreprocDefined name ->
            PreprocDefined <$> recurse name
        PreprocScopedDefine define stmts undef ->
            PreprocScopedDefine <$> recurse define <*> recurse stmts <*> recurse undef
        MacroBodyStmt stmts ->
            MacroBodyStmt <$> recurse stmts
        MacroBodyFunCall expr ->
            MacroBodyFunCall <$> recurse expr
        MacroParam name ->
            MacroParam <$> recurse name
        StaticAssert cond msg ->
            StaticAssert <$> recurse cond <*> recurse msg
        LicenseDecl license copyrights ->
            LicenseDecl <$> recurse license <*> recurse copyrights
        CopyrightDecl from to owner ->
            CopyrightDecl <$> recurse from <*> recurse to <*> recurse owner
        Comment doc start contents end ->
            Comment doc <$> recurse start <*> recurse contents <*> recurse end
        CommentBlock comment ->
            CommentBlock <$> recurse comment
        Commented comment node ->
            Commented <$> recurse comment <*> recurse node
        ExternC decls ->
            ExternC <$> recurse decls
        CompoundStmt stmts ->
            CompoundStmt <$> recurse stmts
        Break ->
            pure Break
        Goto label ->
            Goto <$> recurse label
        Continue ->
            pure Continue
        Return value ->
            Return <$> recurse value
        SwitchStmt value cases ->
            SwitchStmt <$> recurse value <*> recurse cases
        IfStmt cond thenStmts elseStmt ->
            IfStmt <$> recurse cond <*> recurse thenStmts <*> recurse elseStmt
        ForStmt initStmt cond next stmts ->
            ForStmt <$> recurse initStmt <*> recurse cond <*> recurse next <*> recurse stmts
        WhileStmt cond stmts ->
            WhileStmt <$> recurse cond <*> recurse stmts
        DoWhileStmt stmts cond ->
            DoWhileStmt <$> recurse stmts <*> recurse cond
        Case value stmt ->
            Case <$> recurse value <*> recurse stmt
        Default stmt ->
            Default <$> recurse stmt
        Label label stmt ->
            Label <$> recurse label <*> recurse stmt
        VLA ty name size ->
            VLA <$> recurse ty <*> recurse name <*> recurse size
        VarDecl ty decl ->
            VarDecl <$> recurse ty <*> recurse decl
        Declarator spec value ->
            Declarator <$> recurse spec <*> recurse value
        DeclSpecVar name ->
            DeclSpecVar <$> recurse name
        DeclSpecArray spec size ->
            DeclSpecArray <$> recurse spec <*> recurse size
        InitialiserList values ->
            InitialiserList <$> recurse values
        UnaryExpr op expr ->
            UnaryExpr op <$> recurse expr
        BinaryExpr lhs op rhs ->
            BinaryExpr <$> recurse lhs <*> pure op <*> recurse rhs
        TernaryExpr cond thenExpr elseExpr ->
            TernaryExpr <$> recurse cond <*> recurse thenExpr <*> recurse elseExpr
        AssignExpr lhs op rhs ->
            AssignExpr <$> recurse lhs <*> pure op <*> recurse rhs
        ParenExpr expr ->
            ParenExpr <$> recurse expr
        CastExpr ty expr ->
            CastExpr <$> recurse ty <*> recurse expr
        CompoundExpr ty expr ->
            CompoundExpr <$> recurse ty <*> recurse expr
        SizeofExpr expr ->
            SizeofExpr <$> recurse expr
        SizeofType ty ->
            SizeofType <$> recurse ty
        LiteralExpr ty value ->
            LiteralExpr ty <$> recurse value
        VarExpr name ->
            VarExpr <$> recurse name
        MemberAccess name field ->
            MemberAccess <$> recurse name <*> recurse field
        PointerAccess name field ->
            PointerAccess <$> recurse name <*> recurse field
        ArrayAccess arr idx ->
            ArrayAccess <$> recurse arr <*> recurse idx
        FunctionCall callee args ->
            FunctionCall <$> recurse callee <*> recurse args
        CommentExpr comment expr ->
            CommentExpr <$> recurse comment <*> recurse expr
        EnumClass name members ->
            EnumClass <$> recurse name <*> recurse members
        EnumConsts name members ->
            EnumConsts <$> recurse name <*> recurse members
        EnumDecl name members tyName ->
            EnumDecl <$> recurse name <*> recurse members <*> recurse tyName
        Enumerator name value ->
            Enumerator <$> recurse name <*> recurse value
        Typedef ty name ->
            Typedef <$> recurse ty <*> recurse name
        TypedefFunction ty ->
            TypedefFunction <$> recurse ty
        Namespace scope name members ->
            Namespace scope <$> recurse name <*> recurse members
        Class scope name tyvars members ->
            Class scope <$> recurse name <*> recurse tyvars <*> recurse members
        ClassForward name tyvars ->
            ClassForward <$> recurse name <*> recurse tyvars
        Struct name members ->
            Struct <$> recurse name <*> recurse members
        Union name members ->
            Union <$> recurse name <*> recurse members
        MemberDecl ty decl width ->
            MemberDecl <$> recurse ty <*> recurse decl <*> recurse width
        TyConst ty ->
            TyConst <$> recurse ty
        TyPointer ty ->
            TyPointer <$> recurse ty
        TyStruct name ->
            TyStruct <$> recurse name
        TyFunc name ->
            TyFunc <$> recurse name
        TyVar name ->
            TyVar <$> recurse name
        TyStd name ->
            TyStd <$> recurse name
        TyUserDefined name ->
            TyUserDefined <$> recurse name
        FunctionDecl scope proto errors ->
            FunctionDecl scope <$> recurse proto <*> recurse errors
        FunctionDefn scope proto body ->
            FunctionDefn scope <$> recurse proto <*> recurse body
        FunctionPrototype ty name params ->
            FunctionPrototype <$> recurse ty <*> recurse name <*> recurse params
        FunctionParam ty decl ->
            FunctionParam <$> recurse ty <*> recurse decl
        Event name params ->
            Event <$> recurse name <*> recurse params
        EventParams params ->
            EventParams <$> recurse params
        Property ty decl accessors ->
            Property <$> recurse ty <*> recurse decl <*> recurse accessors
        Accessor name params errors ->
            Accessor <$> recurse name <*> recurse params <*> recurse errors
        ErrorDecl name errors ->
            ErrorDecl <$> recurse name <*> recurse errors
        ErrorList errors ->
            ErrorList <$> recurse errors
        ErrorFor name ->
            ErrorFor <$> recurse name
        Ellipsis ->
            pure Ellipsis
        ConstDecl ty name ->
            ConstDecl <$> recurse ty <*> recurse name
        ConstDefn scope ty name value ->
            ConstDefn scope <$> recurse ty <*> recurse name <*> recurse value

      where
        recurse :: TraverseAst iattr oattr itext otext a => a -> f (Mapped iattr oattr itext otext a)
        recurse = mapFileAst actions currentFile

instance TraverseAst iattr oattr itext otext [Node iattr (Lexeme itext)] where
    type Mapped iattr oattr itext otext [Node iattr (Lexeme itext)]
                                      = [Node oattr (Lexeme otext)]
    mapFileAst actions@AstActions{..} currentFile = doNodes currentFile <*>
        traverse (mapFileAst actions currentFile)

instance TraverseAst iattr oattr itext otext (FilePath, [Node iattr (Lexeme itext)]) where
    type Mapped iattr oattr itext otext (FilePath, [Node iattr (Lexeme itext)])
                                      = (FilePath, [Node oattr (Lexeme otext)])
    mapFileAst actions@AstActions{..} _ tu@(currentFile, _) = doFile <*>
        traverse (mapFileAst actions currentFile) $ tu

instance TraverseAst iattr oattr itext otext [(FilePath, [Node iattr (Lexeme itext)])] where
    type Mapped iattr oattr itext otext [(FilePath, [Node iattr (Lexeme itext)])]
                                      = [(FilePath, [Node oattr (Lexeme otext)])]
    mapFileAst actions@AstActions{..} currentFile = doFiles <*>
        traverse (mapFileAst actions currentFile)

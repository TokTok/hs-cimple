#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (foldM, when)
import           Data.Char              (isSpace)
import qualified Data.IntSet            as IntSet
import           Data.List              (intercalate, isInfixOf, isPrefixOf,
                                         isSuffixOf, sortOn)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe, isJust, listToMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Options.Applicative    (Parser, argument, execParser, fullDesc,
                                         header, help, helper, info, long,
                                         metavar, progDesc, str, switch)

import           Data.Array             (Array, assocs, (!))
import qualified Data.Array             as Array
import           Data.Char              (isDigit)
import qualified Data.Set               as Set
import           Happy.Frontend         (parseYFileContents)
import           Happy.Frontend.AbsSyn
import           Happy.Frontend.Mangler (mangler)
import           Happy.Grammar          (Name (..), getName)
import qualified Happy.Grammar          as HG
import           Happy.Tabular          (Tables (..), genTables,
                                         select_first_reduction)
import           Happy.Tabular.First    (mkFirst)
import           Happy.Tabular.LALR     (Goto (..), LRAction (..), Lr1Item (..))
import qualified Happy.Tabular.NameSet  as NameSet

-- Re-implementing the core logic using Happy's AST

data Grammar =
    Grammar
        { preambleLines   :: [T.Text]
        , directives      :: [Directive String]
        , startNT         :: String
        , grammarFuncs    :: Map.Map String (Rule String)
        , grammarNonterms :: Map.Map String (Rule String)
        , postambleLines  :: [T.Text]
        }

convertAbsSyn :: BookendedAbsSyn -> Grammar
convertAbsSyn (BookendedAbsSyn _pragma headerLine (AbsSyn dirs rules) footer) =
    Grammar
        { preambleLines = maybe [] (T.lines . T.pack) headerLine
        , directives = dirs
        , startNT = case rules of (Rule name _ _ _) : _ -> name; _ -> ""
        , grammarFuncs = Map.fromList [ (name, r) | r@(Rule name (_:_) _ _) <- rules ]
        , grammarNonterms = Map.fromList [ (name, r) | r@(Rule name [] _ _) <- rules ]
        , postambleLines = maybe [] (T.lines . T.pack) footer
        }

grammarToAbsSyn :: Grammar -> AbsSyn String
grammarToAbsSyn g =
    AbsSyn (directives g) (Map.elems (grammarNonterms g) ++ Map.elems (grammarFuncs g))

totalConflicts :: Grammar -> Int
totalConflicts g =
    let absSyn = grammarToAbsSyn g
    in case mangler "input.y" absSyn of
        Left errs -> error $ unlines (map show errs)
        Right (hg, _, _) ->
            let t = genTables select_first_reduction hg
                (_, (sr, rr)) = conflicts t
            in sr + rr

getNTType :: String -> String
getNTType name
    | "Opt" `isInfixOf` name = "Maybe NonTerm"
    | "List" `isInfixOf` name = "[NonTerm]"
    | otherwise = "NonTerm"

-- We need a way to represent the terms in a production.
-- Happy's Production type: data Production a = Production [Term a] (Maybe String) (Maybe Int) (Maybe [String])
-- Term a can be App a [Term a] | Term a

termToString :: Term -> String
termToString (App f []) = f
termToString (App f args) = f ++ "(" ++ intercalate "," (map termToString args) ++ ")"

usedIndices :: String -> IntSet.IntSet
usedIndices s = go s
  where
    go [] = IntSet.empty
    go ('$':c:cs) | isDigit c =
        let (digits, rest) = span isDigit (c:cs)
        in IntSet.insert (read digits) (go rest)
    go (_:cs) = go cs

isIdToken :: String -> Bool
isIdToken t = t `elem` ["ID_VAR", "ID_SUE_TYPE", "ID_CONST", "ID_FUNC_TYPE", "ID_STD_TYPE"]

termEq :: Term -> Term -> Bool
termEq (App f1 as1) (App f2 as2) = f1 == f2 && length as1 == length as2 && all (uncurry termEq) (zip as1 as2)

instantiateFunc :: Grammar -> String -> String -> (Grammar, String)
instantiateFunc g funcName arg =
    let mangledName = funcName ++ "_" ++ arg ++ "_"
    in if Map.member mangledName (grammarNonterms g)
       then (g, mangledName)
       else
           let func = grammarFuncs g Map.! funcName
               param = case func of Rule _ [p] _ _ -> p; _ -> error $ "func " ++ funcName ++ " has unexpected params"

               newProds =
                   [ Prod (map (instantiateTerm param arg) prods) code line prec
                   | Prod prods code line prec <- case func of Rule _ _ ps _ -> ps ]

               newNT = Rule mangledName [] newProds (case func of Rule _ _ _ doc -> doc)
               newG = g { grammarNonterms = Map.insert mangledName newNT (grammarNonterms g) }
           in (newG, mangledName)
  where
    instantiateTerm p a (App f args)
        | f == p = App a (map (instantiateTerm p a) args)
        | otherwise = App f (map (instantiateTerm p a) args)

instantiateRuleRef :: Grammar -> Term -> (Grammar, Term)
instantiateRuleRef g (App funcName [App arg []])
    | Map.member funcName (grammarFuncs g) =
        let (newG, mangled) = instantiateFunc g funcName arg
        in (newG, App mangled [])
instantiateRuleRef g (App f args) =
    let (newG, newArgs) = foldl step (g, []) args
    in (newG, App f newArgs)
  where
    step (accG, accArgs) arg = let (g', arg') = instantiateRuleRef accG arg in (g', accArgs ++ [arg'])

instantiateGrammar :: Grammar -> Grammar
instantiateGrammar g =
    let (newG, changed) = step g
    in if changed then instantiateGrammar newG else newG
  where
    step g0 = Map.foldlWithKey' ntStep (g0, False) (grammarNonterms g0)

    ntStep (accG, accChanged) name (Rule rname rparam prods rdoc) =
        let (newG, newProds, changed) = foldl prodStep (accG, [], False) prods
        in if changed
               then (newG { grammarNonterms = Map.insert name (Rule rname rparam newProds rdoc) (grammarNonterms newG) }, True)
               else (newG, accChanged)

    prodStep (accG, accProds, accChanged) (Prod terms code line prec) =
        let (newG, newTerms, changed) = foldl termStep (accG, [], False) terms
        in (newG, accProds ++ [Prod newTerms code line prec], accChanged || changed)

    termStep (accG, accTerms, accChanged) t =
        let (newG, t') = instantiateRuleRef accG t
        in (newG, accTerms ++ [t'], accChanged || not (termEq t t'))

showProduction :: Prod String -> String
showProduction (Prod terms code _ prec) =
    "\t" ++ Prelude.unwords (map termToString terms) ++ showPrec prec ++ "\t\t{ " ++ code ++ " }"
  where
    showPrec PrecNone     = ""
    showPrec PrecShift    = " %shift"
    showPrec (PrecId id') = " %prec " ++ id'

showRule :: String -> Rule String -> [String]
showRule name (Rule _ _ prods typ) =
    (name ++ " :: { " ++ fromMaybe (getNTType name) typ ++ " }") :
    name :
        case prods of [] -> []; (p:ps) -> (":" ++ showProduction p) : map (("|" ++) . showProduction) ps

showDirective :: Directive String -> String
showDirective (TokenType t) = "%tokentype { " ++ t ++ " }"
showDirective (TokenSpec specs) = "%token\n" ++ concatMap showTokenSpecFull specs
  where
    showTokenSpecFull (name, spec) = "    " ++ name ++ " { " ++ getSpecStr spec ++ " }\n"
    getSpecStr (TokenFixed s) = s
    getSpecStr _              = ""
showDirective (TokenName name (Just start) partial) = (if partial then "%partial " else "%name ") ++ name ++ " " ++ start
showDirective (TokenName name Nothing partial) = (if partial then "%partial " else "%name ") ++ name
showDirective (TokenLexer lex' er) = "%lexer { " ++ lex' ++ " } { " ++ er ++ " }"
showDirective TokenImportedIdentity = "%importedidentity"
showDirective (TokenMonad "()" monad "Prelude.>>=" "Prelude.return") = "%monad { " ++ monad ++ " }"
showDirective (TokenMonad ty monad bind ret) = "%monad { " ++ ty ++ " } { " ++ monad ++ " } { " ++ bind ++ " } { " ++ ret ++ " }"
showDirective (TokenNonassoc ids) = "%nonassoc " ++ unwords ids
showDirective (TokenRight ids) = "%right " ++ unwords ids
showDirective (TokenLeft ids) = "%left " ++ unwords ids
showDirective (TokenExpect n) = "%expect " ++ show n
showDirective (TokenError err Nothing) = "%error { " ++ err ++ " }"
showDirective (TokenError err typ) = "%error { " ++ err ++ " }" ++ maybe "" (\t -> " { " ++ t ++ " }") typ
showDirective TokenErrorExpected = "%error.expected"
showDirective (TokenErrorHandlerType t) = "%errorhandlertype " ++ t
showDirective (TokenAttributetype t) = "%attributetype { " ++ t ++ " }"
showDirective (TokenAttribute name val) = "%attribute " ++ name ++ " { " ++ val ++ " }"

printGrammar :: Grammar -> [T.Text]
printGrammar g =
    (if null (preambleLines g) then [] else ["{"] ++ preambleLines g ++ ["}"]) ++
        map (T.pack . showDirective) (directives g) ++
        ["%%"] ++
        concatMap (map T.pack) (showNonTerms (startNT g) (grammarNonterms g)) ++

    (if null (postambleLines g) then [] else ["{"] ++ postambleLines g ++ ["}"])
  where
    showNonTerms start nts =
        case Map.lookup start nts of
            Nothing -> map (uncurry showRule) (sortOn fst (Map.toList nts))
            Just startRule ->
                let rest = Map.delete start nts in showRule start startRule : map (uncurry showRule) (sortOn fst (Map.toList rest))

isSignificant :: String -> Bool
isSignificant name =
    not ("List" `isInfixOf` name) &&
    not ("Opt" `isInfixOf` name) &&
    not ("Tokens" `isInfixOf` name) &&
    not ("Words" `isInfixOf` name) &&
    not ("_inner" `isSuffixOf` name) &&
    not ("push_" `isPrefixOf` name) &&
    not (all (=='_') name) &&
    not (any (`isInfixOf` name)
             [ "AssignExpr", "InitialiserExpr", "PureExprOps"
             , "PreprocSafeExpr", "PreprocAtoms"
             ]) &&
    not (name `elem` [ "Term", "CommentToken", "CommentWord", "IgnoreBody"
                     , "Stmts", "ToplevelDecls", "Initialisers", "Args"
                     ])

tupleName :: Int -> String
tupleName i = "v" ++ show i

demangle :: String -> String
demangle s =
    if "_" `isSuffixOf` s
    then let s' = reverse $ dropWhile (=='_') $ reverse s
         in case break (== '_') s' of
            (base, '_':rest) -> base ++ "(" ++ rest ++ ")"
            (base, _)        -> base
    else s

updateProd :: Int -> IntSet.IntSet -> Maybe Int -> String -> Prod String -> Prod String
updateProd pivot used namingIdx headerNT (Prod terms code line prec) =
    let (_, after) = splitAt pivot terms
        newTerms = App headerNT [] : after

        -- Indices in the prefix that are included in the header's return tuple.
        allUsed = maybe used (`IntSet.insert` used) namingIdx
        prefixUsed = [ i | i <- IntSet.toList allUsed, i <= pivot ]

        -- Pattern for the tuple: (v1, v2, ...)
        tuplePat = if null prefixUsed then "()" else "(" ++ intercalate "," (map tupleName prefixUsed) ++ ")"

        -- Transform the code:
        -- 1. For $i where i <= pivot, replace with tupleName i
        -- 2. For $i where i > pivot, replace with $(i - pivot + 1)
        transformCode [] = []
        transformCode ('$':c:cs) | isDigit c =
            let (digits, rest) = span isDigit (c:cs)
                val = read digits :: Int
            in if val <= pivot
               then tupleName val ++ transformCode rest
               else '$' : show (val - pivot + 1) ++ transformCode rest
        transformCode (c:cs) = c : transformCode cs

        (isMonadic, cleanCode) = case dropWhile isSpace code of
            ('%':rest) -> (True, rest)
            rest       -> (False, rest)

        transformedClean = transformCode cleanCode

        monadicCode = if isMonadic
                      then "(" ++ transformedClean ++ ")"
                      else "return (" ++ transformedClean ++ ")"

        finalCode =
            "% " ++ (if null prefixUsed then "" else "let " ++ tuplePat ++ " = $1 in ")
            ++ monadicCode ++ " >>= \\res -> popContext >> return res"
    in Prod newTerms finalCode line prec

getTermType :: Map.Map String (Rule String) -> Term -> String
getTermType nts (App name _) =
    case Map.lookup name nts of
        Just (Rule _ _ _ (Just t)) -> t
        Just (Rule _ _ _ Nothing)  -> getNTType name
        Nothing                    -> "Term" -- It's a terminal

generateHeaderRule :: Map.Map String (Rule String) -> String -> String -> Int -> IntSet.IntSet -> Maybe Int -> [Term] -> Rule String
generateHeaderRule nts name headerNT pivot used namingIdx terms =
    let -- What to push?
        contextName = demangle name
        pushValue = case namingIdx of
            Just i ->
                let t = terms !! (i-1)
                    typ = getTermType nts t
                in if typ == "Term"
                   then "(ContextLexeme " ++ show contextName ++ " v" ++ show i ++ ")"
                   else if "[" `isPrefixOf` typ
                   then "(case listToMaybe v" ++ show i ++ " of { Just n -> ContextNode " ++ show contextName ++ " n; Nothing -> Context " ++ show contextName ++ " })"
                   else "(ContextNode " ++ show contextName ++ " v" ++ show i ++ ")"
            Nothing -> "(Context " ++ show contextName ++ ")"

        -- What to return?
        allUsed = maybe used (`IntSet.insert` used) namingIdx
        prefixUsed = [ i | i <- IntSet.toList allUsed, i <= pivot ]

        -- Transform the headerCode to rebind the used elements so pushExpr can use them
        headerCode = case prefixUsed of
            [] -> "% pushContext " ++ pushValue ++ " >> return ()"
            _  -> let bindings = [ "v" ++ show i ++ " <- return $" ++ show i | i <- prefixUsed ]
                      ret = "(" ++ intercalate "," (map (('$' :) . show) prefixUsed) ++ ")"
                  in "% do { " ++ intercalate "; " (bindings ++ ["pushContext " ++ pushValue, "return " ++ ret]) ++ " }"

        -- Construct the tuple type
        tupleType = if null prefixUsed
                    then "()"
                    else "(" ++ intercalate "," [ getTermType nts (terms !! (i-1)) | i <- prefixUsed ] ++ ")"

    in Rule headerNT [] [Prod terms headerCode 0 PrecNone] (Just tupleType)

updateRuleByIndices :: [Int] -> String -> Int -> IntSet.IntSet -> Maybe Int -> Rule String -> Rule String
updateRuleByIndices targetRuleIndices headerNT pivot used namingIdx (Rule rname params prods typ) =
    Rule rname params (zipWith update [0..] prods) typ
  where
    update ruleNo prod
        | ruleNo `elem` targetRuleIndices = updateProd pivot used namingIdx headerNT prod
        | otherwise = prod

performGroupInjection :: PivotAnalysis -> Grammar -> ([String], String) -> [Int] -> Grammar
performGroupInjection _ g (prefixStrings, ntName) localRuleIndices =
    case localRuleIndices of
      [] -> g
      (firstIdx:_) ->
        let pivot = length prefixStrings
            headerNT = ntName ++ "_h" ++ show firstIdx
            nts = grammarNonterms g
            Rule _ _ prods _ = nts Map.! ntName

            -- Use the terms from the first production in the group
            exampleProd = prods !! firstIdx
            Prod exampleTerms _ _ _ = exampleProd
            prefixTerms = take pivot exampleTerms

            targetProds = [ prods !! i | i <- localRuleIndices ]

            -- Union of all used indices in the prefix across all rules in the group
            used = IntSet.unions [ usedIndices code | Prod _ code _ _ <- targetProds ]

            -- Naming: try to find a naming index that works for at least one rule.
            -- We'll just take the naming index of the first rule for now.
            namingIdx = getNamingIndexForProd nts pivot exampleProd

            headerRule = generateHeaderRule nts ntName headerNT pivot used namingIdx prefixTerms

            newNonterms = Map.adjust (updateRuleByIndices localRuleIndices headerNT pivot used namingIdx) ntName nts
            newG = g { grammarNonterms = Map.insert headerNT headerRule newNonterms }
        in newG

getNamingIndex :: Map.Map String (Rule String) -> Int -> Rule String -> Maybe Int
getNamingIndex nts pivot (Rule _ _ (p:_) _) = getNamingIndexForProd nts pivot p
getNamingIndex _ _ _                        = Nothing

getNamingIndexForProd :: Map.Map String (Rule String) -> Int -> Prod String -> Maybe Int
getNamingIndexForProd nts pivot (Prod terms code _ _) =
    let used = usedIndices code
        ids = [ i | i <- IntSet.toList used, i <= pivot, i <= length terms, isIdToken (termToString (terms !! (i-1))) ]
    in case listToMaybe (reverse ids) of
        Just i -> Just i
        Nothing -> if pivot >= 1 && isTerminal (terms !! 0) then Just 1 else Nothing
  where
    isTerminal (App name _) = not (Map.member name nts)

groupCandidates :: PivotAnalysis -> [(String, Int, Rule String)] -> Map.Map ([String], String) [Int]
groupCandidates _ candidates =
    Map.fromListWith (++)
    [ ((prefix, name), [localRuleNo])
    | (name, pivot, Rule _ _ prods _) <- candidates
    , pivot > 0
    , (localRuleNo, Prod lhs _ _ _) <- zip [0..] prods
    , let prefix = map termToString (take pivot lhs)
    ]

baselineAnalysis :: Grammar -> PivotAnalysis
baselineAnalysis g =
    case mangler "input.y" (grammarToAbsSyn g) of
        Left errs -> error $ unlines (map show errs)
        Right (hg, _, _) -> buildPivotAnalysis hg (genTables select_first_reduction hg)

injectContexts :: Grammar -> IO (Grammar, Int)
injectContexts g = do
    let monad = case [ m | TokenMonad _ m _ _ <- directives g ] of
                    [m] -> m
                    _   -> ""
    if monad /= "Alex"
       then return (g, 0)
       else do
           let baseline = totalConflicts g
               nts = Map.toList (grammarNonterms g)
               pa = baselineAnalysis g
           let candidates = [ (name, findSafePivot (grammarNonterms g) pa name r, r)
                            | (name, r) <- nts, isSignificant name ]

           -- mapM_ (\(name, pivot, _) -> when (pivot > 0) $ putStrLn $ "NT " ++ name ++ " pivot " ++ show pivot) candidates

           putStrLn $ "Baseline conflicts: " ++ show baseline
           putStrLn $ "Significant NTs: " ++ show (length candidates)

           let groups = groupCandidates pa candidates
           putStrLn $ "Header groups: " ++ show (Map.size groups)

           let injected = Map.foldlWithKey (performGroupInjection pa) g groups
           let finalConflicts = totalConflicts injected
           putStrLn $ "Final conflicts: " ++ show finalConflicts

           let withPostamble = injected { postambleLines = map (T.replace "preprocessorEnabled = False" "preprocessorEnabled = True") (postambleLines injected) }
           return (addLexerImports withPostamble, Map.size groups)

addLexerImports :: Grammar -> Grammar
addLexerImports g =
    g { preambleLines = map updateImport (preambleLines g) }
  where
    updateImport line =
        let s = T.unpack line
        in if "Language.Cimple.Lexer" `isInfixOf` s && "(" `isInfixOf` s
           then T.pack $ sub "(" "(Context(..), getContext, popContext, pushContext, " s
           else line

    sub old new s =
        case old of
            [] -> s
            (o:_) ->
                case break (== o) s of
                    (before, rest) ->
                        if old `isPrefixOf` rest
                        then before ++ new ++ drop (length old) rest
                        else before ++ case rest of
                                         []     -> []
                                         (x:xs) -> x : sub old new xs

data Options = Options
    { inputFile       :: FilePath
    , outputFile      :: FilePath
    , dumpStates      :: Bool
    , dumpConflicts   :: Bool
    , dumpTokenNames  :: Bool
    , dumpPivots      :: Bool
    , dumpProductions :: Bool
    , dumpDominators  :: Bool
    }

options :: Parser Options
options = Options
    <$> argument str (metavar "INPUT" <> help "Input .y file")
    <*> argument str (metavar "OUTPUT" <> help "Output .y file")
    <*> switch (long "dump-states" <> help "Dump the LALR states")
    <*> switch (long "dump-conflicts" <> help "Dump the grammar conflicts")
    <*> switch (long "dump-token-names" <> help "Dump the token names")
    <*> switch (long "dump-pivots" <> help "Dump the pivot analysis")
    <*> switch (long "dump-productions" <> help "Dump the productions")
    <*> switch (long "dump-dominators" <> help "Dump the dominator analysis")

data PivotAnalysis = PivotAnalysis
    {
      tokenToId    :: Map.Map String Int
    , tables       :: Tables
    , happyGrammar :: HG.Grammar String
    , rulesForNT   :: Map.Map String [Int]
    , ntOwners     :: Map.Map String (Set.Set String)
    , recursiveNTs :: Set.Set String
    }

buildPivotAnalysis :: HG.Grammar String -> Tables -> PivotAnalysis
buildPivotAnalysis hg t =
    let tokenNames = HG.token_names hg
        prods = HG.productions hg

        -- Build the grammar graph: which NTs use which other NTs?
        graph = Map.fromListWith Set.union
                [ (tokenNames ! nt, Set.fromList [ tokenNames ! lhs_nt | lhs_nt <- lhs, lhs_nt < HG.first_term hg ])
                | HG.Production nt lhs _ _ <- prods ]

        allNTs = Map.keys graph
        significantNTs = Set.fromList (filter isSignificant allNTs)

        -- Transpose graph: who uses me?
        usedBy = Map.fromListWith Set.union
                 [ (child, Set.singleton parent)
                 | (parent, children) <- Map.toList graph
                 , child <- Set.toList children ]

        -- Reachability for recursion detection
        initialReach = Map.fromList [ (nt, Map.findWithDefault Set.empty nt graph) | nt <- allNTs ]
        stepReach reach = Map.fromList
            [ (nt, Set.unions (r : [ Map.findWithDefault Set.empty child reach | child <- Set.toList r ]))
            | (nt, r) <- Map.toList reach ]
        findReach o = let o' = stepReach o in if o == o' then o else findReach o'
        reachability = findReach initialReach

        recursive = Set.fromList [ nt | (nt, r) <- Map.toList reachability, Set.member nt r ]

        -- Calculate owners (Significant ancestors) using a simple fixed-point
        initialOwners = Map.fromList [ (nt, if Set.member nt significantNTs then Set.singleton nt else Set.empty) | nt <- allNTs ]

        step owners = Map.fromList
            [ (nt, Set.unions (Map.findWithDefault Set.empty nt owners :
                               [ Map.findWithDefault Set.empty parent owners | parent <- Set.toList (Map.findWithDefault Set.empty nt usedBy) ]))
            | nt <- allNTs ]

        -- Fixed point (grammar is small, this is fast enough)
        findFixedPoint o = let o' = step o in if o == o' then o else findFixedPoint o'
        finalOwners = findFixedPoint initialOwners

        ntToRules = Map.fromListWith (++) [ (tokenNames ! nt, [i]) | (i, HG.Production nt _ _ _) <- zip [0..] prods ]
    in PivotAnalysis
        { tokenToId = Map.fromList [ (v, getName k) | (k, v) <- Array.assocs (HG.token_names hg) ]
        , tables    = t
        , happyGrammar = hg
        , rulesForNT = ntToRules
        , ntOwners = finalOwners
        , recursiveNTs = recursive
        }

findStatesWithItem :: PivotAnalysis -> Int -> Int -> [Int]
findStatesWithItem pa ruleNo dot =
    [ i | (i, (items, _)) <- zip [0..] (lr1items (tables pa))
        , any (\r -> case r of Lr1 r' d' _ -> r' == ruleNo && d' == dot) items ]

isSafePivot :: PivotAnalysis -> String -> Int -> Bool
isSafePivot pa targetNT pivot =
    let rs = fromMaybe [] (Map.lookup targetNT (rulesForNT pa))
        hg = happyGrammar pa
        tokenNames = HG.token_names hg

        -- Don't inject into recursive rules
        isRec = Set.member targetNT (recursiveNTs pa)

        -- All states reachable by this pivot for any rule of this NT
        targetStates = concatMap (\ruleNo -> findStatesWithItem pa ruleNo pivot) rs

        isPure s =
            let (items, _) = (lr1items (tables pa)) !! s

                -- For every item in the state, targetNT must be an ancestor.
                itemLHS = [ tokenNames ! nt | Lr1 r _ _ <- items, let HG.Production nt _ _ _ = HG.lookupProdNo hg r ]
                itemOwners = [ Map.findWithDefault Set.empty lhs (ntOwners pa) | lhs <- itemLHS ]
            in not (null items) && all (Set.member targetNT) itemOwners

    in pivot > 0 && not isRec && not (null targetStates) && all isPure targetStates

findSafePivot :: Map.Map String (Rule String) -> PivotAnalysis -> String -> Rule String -> Int
findSafePivot nts pa name r@(Rule _ _ prods _) =
    let terms = case prods of (Prod ts _ _ _ : _) -> map termToString ts; _ -> []
        -- Try all indices from 1 to length.
        safeIndices = [ i | i <- [1 .. length terms], isSafePivot pa name i ]
        -- Prefer indices that have a naming index (capture a name).
        withNaming = [ i | i <- safeIndices, isJust (getNamingIndex nts i r) ]
    in fromMaybe 0 $ listToMaybe (withNaming ++ safeIndices)

main :: IO ()
main = do
    opts <- execParser optsInfo
    start <- getCurrentTime
    content <- readFile (inputFile opts)
    case parseYFileContents content of
        Left err -> error err
        Right bookendedAbsSyn@(BookendedAbsSyn _ _ _ _) -> do
            let grammar = convertAbsSyn bookendedAbsSyn
            let instantiated = instantiateGrammar grammar
            case mangler (inputFile opts) (grammarToAbsSyn instantiated) of
                Left errs -> error $ unlines (map show errs)
                Right (hg, _, _) -> do
                    let t = genTables select_first_reduction hg
                    when (dumpStates opts) $ mapM_ print (lr1items t)
                    when (dumpConflicts opts) $ print (conflicts t)
                    when (dumpTokenNames opts) $ do
                        let names = Array.assocs (HG.token_names hg)
                        mapM_ (\r -> putStrLn $ show (getName (fst r)) ++ ": " ++ snd r) names
                    when (dumpProductions opts) $ do
                        let prods = zip [(0 :: Int)..] (HG.productions hg)
                            names = HG.token_names hg
                        mapM_ (\(i, HG.Production nt lhs _ _) ->
                                putStrLn $ show i ++ ": " ++ (names ! nt) ++ " -> " ++ unwords (map (names !) lhs)
                              ) prods

                    let pa = buildPivotAnalysis hg t
                    when (dumpDominators opts) $ do
                        putStrLn "Non-terminal Owners (Significant Ancestors):"
                        mapM_ (\(nt, owners) -> putStrLn $ "  " ++ nt ++ ": " ++ show (Set.toList owners)) (Map.toList (ntOwners pa))
                        putStrLn "\nState Dominators:"
                        mapM_ (printStateDominator pa) [0 .. length (lr1items t) - 1]


                    when (dumpPivots opts) $ do
                        let nts = Map.toList (grammarNonterms instantiated)
                        let candidates = [ (name, findSafePivot (grammarNonterms instantiated) pa name r, r)
                                         | (name, r) <- nts, isSignificant name ]
                        mapM_ (printPivot (grammarNonterms instantiated) pa) candidates

                    if dumpStates opts || dumpConflicts opts || dumpTokenNames opts || dumpPivots opts || dumpProductions opts || dumpDominators opts
                       then return ()
                       else do
                           (injected, n) <- injectContexts instantiated
                           TIO.writeFile (outputFile opts) (T.unlines (printGrammar injected))
                           end <- getCurrentTime
                           putStrLn $ "Time taken: " ++ show (diffUTCTime end start) ++ ", inserted " ++ show n ++ " contexts"
    where
      optsInfo = info (helper <*> options)
          ( fullDesc
         <> progDesc "Instantiate Happy grammar functions and inject lexer contexts"
         <> header "cimple-yacc - Happy grammar preprocessor" )

      printPivot nts pa (name, idx, Rule rname rparams prods rdoc) = do
          let terms = case prods of (Prod ts _ _ _ : _) -> map termToString ts; _ -> []
          let states = case Map.lookup name (rulesForNT pa) of
                          Just rs -> concatMap (\ruleNo -> findStatesWithItem pa ruleNo idx) rs
                          Nothing -> []
          let namingIdx = getNamingIndex nts idx (Rule rname rparams prods rdoc)
          putStrLn $ "NT: " ++ name ++ " at " ++ show idx ++ " (naming: " ++ show namingIdx ++ ") (prefix: " ++ show (take idx terms) ++ ") -> States: " ++ show states
          mapM_ (dumpStateItems pa) states

      printStateDominator pa s = do
          let (items, _) = (lr1items (tables pa)) !! s
              names = HG.token_names (happyGrammar pa)
              owners = Set.unions [ Map.findWithDefault Set.empty (names ! nt) (ntOwners pa)
                                  | Lr1 r _ _ <- items
                                  , let HG.Production nt _ _ _ = HG.lookupProdNo (happyGrammar pa) r ]
          putStrLn $ "  State " ++ show s ++ " dominated by: " ++ show (Set.toList owners)

      dumpStateItems pa s = do
          let (items, _) = (lr1items (tables pa)) !! s
          putStrLn $ "  State " ++ show s ++ " items:"
          mapM_ (printItem pa) items

      printItem pa (Lr1 r d _) = do
          let HG.Production nt lhs _ _ = HG.lookupProdNo (happyGrammar pa) r
              names = HG.token_names (happyGrammar pa)
              (before, after) = splitAt d lhs
          putStrLn $ "    " ++ (names ! nt) ++ " -> " ++ unwords (map (names !) before) ++ " . " ++ unwords (map (names !) after)

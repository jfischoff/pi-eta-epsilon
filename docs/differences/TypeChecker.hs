                                                        (
module Language.LBNF.TypeChecker where                  | module TypeChecker where
                                                        (
import Control.Monad                                    (
import Data.List                                        (
import Data.Char                                        (
                                                        (
import Language.LBNF.CF                                 | import CF
import Language.LBNF.Runtime                            | import ErrM
                                                        (
data Base = BaseT String                                (
          | ListT Base                                  (
    deriving (Eq)                                       (
                                                        (
data Type = FunT [Base] Base                            (
    deriving (Eq)                                       (
                                                        (
instance Show Base where                                (
    show (BaseT x) = x                                  (
    show (ListT t) = "[" ++ show t ++ "]"               (
                                                        (
instance Show Type where                                (
    show (FunT ts t) = unwords $ map show ts ++ ["->",  (
                                                        (
data Context = Ctx  { ctxLabels :: [(String, Type)]     (
                    , ctxTokens :: [String]             (
                    }                                   (
                                                        (
catchErr :: ParseMonad a -> (String -> ParseMonad a) -> | catchErr :: Err a -> (String -> Err a) -> Err a
catchErr (Bad s) f = f s                                (
catchErr (Ok x) _  = Ok x                               (
                                                        (
buildContext :: CF -> Context                           (
buildContext cf@(_,rules) =                             | buildContext cf@(CFG(_,rules)) =
    Ctx                                                 (
    [ (f, mkType cat args) | (f,(cat,args)) <- rules    |     [ (f, mkType cat args) | Rule (f,(cat,args)) <- rul
                           , not (isCoercion f)         (
                           , not (isNilCons f)          (
    ]                                                   (
    ("Ident" : tokenNames cf)                           (
  where                                                 (
                                                        (
    mkType cat (Left args) = FunT [ mkBase t | Left t < |     mkType cat args = FunT [ mkBase t | Left t <- args,
                                  (mkBase cat)          (
    mkType cat (Right reg) = FunT [ BaseT "String" ] (m <
    mkBase t                                            (
        | isList t  = ListT $ mkBase $ normCatOfList t  (
        | otherwise = BaseT $ normCat t                 (
                                                        (
isToken :: String -> Context -> Bool                    (
isToken x ctx = elem x $ ctxTokens ctx                  (
                                                        (
extendContext :: Context -> [(String,Type)] -> Context  (
extendContext ctx xs = ctx { ctxLabels = xs ++ ctxLabel (
                                                        (
lookupCtx :: String -> Context -> ParseMonad Type       | lookupCtx :: String -> Context -> Err Type
lookupCtx x ctx                                         (
    | isToken x ctx = return $ FunT [BaseT "String"] (B (
    | otherwise     =                                   (
    case lookup x $ ctxLabels ctx of                    (
        Nothing -> fail $ "Undefined symbol '" ++ x ++  (
        Just t  -> return t                             (
                                                        (
checkDefinitions :: CF -> ParseMonad ()                 | checkDefinitions :: CF -> Err ()
checkDefinitions cf@((ps,_),_) =                        | checkDefinitions cf =
    do  checkContext ctx                                (
        sequence_ [ checkDefinition ctx f xs e | FunDef |         sequence_ [checkDefinition ctx f xs e | FunDef 
    where                                               (
        ctx = buildContext cf                           (
                                                        (
checkContext :: Context -> ParseMonad ()                | checkContext :: Context -> Err ()
checkContext ctx =                                      (
    mapM_ checkEntry $ groupSnd $ ctxLabels ctx         (
    where                                               (
        -- This is a very handy function which transfor (
        -- with duplicate keys to a list valued lookup  (
        -- keys.                                        (
        groupSnd :: Ord a => [(a,b)] -> [(a,[b])]       (
        groupSnd =                                      (
            map ((fst . head) /\ map snd)               (
            . groupBy ((==) **.* fst)                   (
            . sortBy (compare **.* fst)                 (
                                                        (
        (f /\ g) x     = (f x, g x)                     (
        (f **.* g) x y = f (g x) (g y)                  (
                                                        (
        checkEntry (f,ts) =                             (
            case nub ts of                              (
                [_] -> return ()                        (
                ts' ->                                  (
                    fail $ "The symbol '" ++ f ++ "' is (
                            unlines (map (("  " ++) . s (
                                                        (
checkDefinition :: Context -> String -> [String] -> Exp | checkDefinition :: Context -> String -> [String] -> Exp
checkDefinition ctx f xs e =                            (
    do  checkDefinition' dummyConstructors ctx f xs e   (
        return ()                                       (
                                                        (
data ListConstructors = LC                              (
        { nil   :: Base -> String                       (
        , cons  :: Base -> String                       (
        }                                               (
                                                        (
dummyConstructors :: ListConstructors                   (
dummyConstructors = LC (const "[]") (const "(:)")       (
                                                        (
checkDefinition' :: ListConstructors -> Context -> Stri | checkDefinition' :: ListConstructors -> Context -> Stri
checkDefinition' list ctx f xs e =                      (
    do  unless (isLower $ head f) $ fail "Defined funct (
        t@(FunT ts t') <- lookupCtx f ctx `catchErr` \_ (
                                fail $ "'" ++ f ++ "' m (
        let expect = length ts                          (
            given  = length xs                          (
        unless (expect == given) $ fail $ "'" ++ f ++ " (
        e' <- checkExp list (extendContext ctx $ zip xs (
        return (zip xs ts, (e', t'))                    (
    `catchErr` \err -> fail $ "In the definition " ++ u (
    where                                               (
        plural 1 = ""                                   (
        plural _ = "s"                                  (
                                                        (
checkExp :: ListConstructors -> Context -> Exp -> Base  | checkExp :: ListConstructors -> Context -> Exp -> Base 
checkExp list ctx (App "[]" []) (ListT t) = return (App (
checkExp _ _      (App "[]" _) _          = fail $ "[]  (
checkExp list ctx (App "(:)" [e,es]) (ListT t) =        (
    do  e'  <- checkExp list ctx e t                    (
        es' <- checkExp list ctx es (ListT t)           (
        return $ App (cons list t) [e',es']             (
checkExp _ _ (App "(:)" es) _   = fail $ "(:) takes 2 a (
checkExp list ctx e@(App x es) t =                      (
    do  FunT ts t' <- lookupCtx x ctx                   (
        es' <- matchArgs ctx es ts                      (
        unless (t == t') $ fail $ show e ++ " has type  (
        return $ App x es'                              (
    where                                               (
        matchArgs ctx es ts                             (
            | expect /= given   = fail $ "'" ++ x ++ "' (
            | otherwise         = zipWithM (checkExp li (
            where                                       (
                expect = length ts                      (
                given  = length es                      (
checkExp _ _ e@(LitInt _) (BaseT "Integer")     = retur (
checkExp _ _ e@(LitDouble _) (BaseT "Double")   = retur (
checkExp _ _ e@(LitChar _) (BaseT "Char")       = retur (
checkExp _ _ e@(LitString _) (BaseT "String")   = retur (
checkExp _ _ e t = fail $ show e ++ " does not have typ (
                                                        (

{-                                                      (
    BNF Converter: Abstract syntax                      (
    Copyright (C) 2004  Author: Markus Forsberg, Aarne  (
                                                        (
    This program is free software; you can redistribute (
    it under the terms of the GNU General Public Licens (
    the Free Software Foundation; either version 2 of t (
    (at your option) any later version.                 (
                                                        (
    This program is distributed in the hope that it wil (
    but WITHOUT ANY WARRANTY; without even the implied  (
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE (
    GNU General Public License for more details.        (
                                                        (
    You should have received a copy of the GNU General  (
    along with this program; if not, write to the Free  (
    Foundation, Inc., 59 Temple Place, Suite 330, Bosto (
-}                                                      (
                                                        (
                                                        (
module Language.LBNF.GetCF where                        | module GetCF(tryReadCF,tryReadCFP,
                                                        >   formatOptC,formatOptCPP,formatOptCPP_STL,
                                                        >   formatOptCSharp,formatOptFSharp,formatOptHaskell,form
                                                        >     formatOptJava15,formatOptJava,formatOptOCAML,format
                                                        >   ) where
                                                        (
import Control.Monad ( when )                           (
                                                        (
import Language.LBNF.CF                                 | import CF
import Language.LBNF.Utils                              | import Utils
-- import Language.LBNF.ParBNF                          | import ParBNF
import Language.LBNF.Grammar(pGrammar, tokens)          <
                                                        <
import Data.List(nub,partition)                         (
import qualified Language.LBNF.Grammar as Abs           | import qualified AbsBNF as Abs
import Language.LBNF.Runtime                            | -- import LexBNF
                                                        > import ErrM
import Data.Char                                        (
import Language.LBNF.TypeChecker                        | import TypeChecker
                                                        (
type TempRHS = Either [Either String String] Reg        | readCF :: ReadOptions -> FilePath -> IO CF
type TempRule = (Fun,(Cat,TempRHS))                     | readCF opts f = tryReadCF opts f >>= return . fst
                                                        (
getCF :: String -> (CF, [String])                       | type ReadOptions = [String]
getCF = getCFofG . pGrammar . tokens                    | isOpt  opts v  = elem v opts
                                                        > anyOpt opts vs = any (isOpt opts) vs
                                                        > allOpt opts vs = all (isOpt opts) vs
                                                        >
                                                        > [formatOptC,formatOptCPP,formatOptCPP_STL,
                                                        >   formatOptCSharp,formatOptFSharp,formatOptHaskell,form
                                                        >     formatOptJava15,formatOptJava,formatOptOCAML,format
                                                        >   ["formatOptC","formatOptCPP","formatOptCPP_STL",
                                                        >     "formatOptCSharp","formatOptFSharp","formatOptHaske
                                                        >       "formatOptJava15","formatOptJava","formatOptOCAML
                                                        >
                                                        >
                                                        > tryReadCF :: ReadOptions -> FilePath -> IO (CF,Bool)
                                                        > tryReadCF opts file = do
                                                        >   (cfp,m) <- tryReadCFP opts file
                                                        >   return (cfp2cf cfp, m)
                                                        >
                                                        > tryReadCFP :: ReadOptions -> FilePath -> IO (CFP,Bool)
                                                        > tryReadCFP opts file = do
                                                        >   putStrLn $ "\nReading grammar from " ++ file
                                                        >   s <- readFile file
                                                        >   let (cfp,msgs1) = getCFP s
                                                        >       cf = cfp2cf cfp
                                                        >       msgs2 = case checkDefinitions cf of
                                                        >                 Bad err -> [err]
                                                        >                 Ok ()   -> []
                                                        >       msgs3 = checkTokens cf
                                                        >       msg = msgs1++msgs2 -- ++ msgs3 -- in a future ver
                                                        >       ret = cfp
                                                        >
                                                        >   let reserved = if anyOpt opts [formatOptJava,formatOp
                                                        >                    then [takeWhile (/='.') file] else [
                                                        >   case filter (not . isDefinedRule) $ notUniqueNames re
                                                        >     ns@(_:_) 
                                                        >       | not (anyOpt opts [formatOptHaskell,formatOptHas
                                                        >         putStrLn $ "ERROR: names not unique: " ++ unwor
                                                        >         return (ret,False)
                                                        >     ns -> do
                                                        >       case ns of
                                                        >         _:_ -> do
                                                        >           putStrLn $ "Warning: names not unique: " ++ u
                                                        >           putStrLn "This can be an error in other back 
                                                        >         _ -> return ()
                                                        >       putStrLn $ unlines msgs3
                                                        >       if not (null msg) then do
                                                        >          putStrLn $ unlines msg
                                                        >          return (ret,False)
                                                        >        else do
                                                        >          putStrLn $ show (length (rulesOfCF cf)) +++ "r
                                                        >          let c3s = [(b,e) | (b,e) <- fst (comments cf),
                                                        >          if null c3s then return () else do
                                                        >            putStrLn 
                                                        >              "Warning: comment delimiters longer than 2
                                                        >            mapM_ putStrLn [b +++ "-" +++ e | (b,e) <- c
                                                        >          return (ret,True)
                                                        >
                                                        > {-
                                                        >     case filter (not . isDefinedRule) $ notUniqueFuns c
                                                        >      [] -> case (badInheritence cf) of
                                                        >        [] -> return (ret,True)
                                                        >        xs -> do
                                                        >         putStrLn "Warning :"
                                                        >         putStrLn $ "  Bad Label name in Category(s) :" 
                                                        >         putStrLn $ "  These categories have more than o
                                                        >         putStrLn $ "  Labels has the same name as the C
                                                        >         putStrLn $ "  certainly cause problems in langu
                                                        >         return (ret,True)
                                                        >      xs -> do  
                                                        >        putStrLn $ "Warning :" 
                                                        >        putStrLn $ "  Non-unique label name(s) : " ++ un
                                                        >        putStrLn $ "  There may be problems with the pre
                                                        >        case (badInheritence cf) of
                                                        >          [] -> return (ret,True)
                                                        >          xs -> do
                                                        >           putStrLn $ "Warning :"
                                                        >           putStrLn $ "  Bad Label name in Category(s) :
                                                        >           putStrLn $ "  These categories have more than
                                                        >           putStrLn $ "  Labels has the same name as the
                                                        >           putStrLn $ "  certainly cause problems in lan
                                                        >           return (ret,True)
                                                        > -}
                                                        (
getCFofG :: ParseMonad Abs.Grammar -> (CF, [String])    | getCF :: String -> (CF, [String])
getCFofG g = (cf,msgs ++ msgs1) where                   | getCF s = let (cfp,msg) = getCFP s in (cfp2cf cfp, msg)
                                                        (
  (cf,msgs1) = ((exts,ruls2),msgs2)                     | getCFP :: String -> (CFP, [String])
  (ruls2,msgs2) = untag $ map (checkRule cf0) $ rulesOf | getCFP s = (cf,msgs ++ msgs1) where
  untag :: ([Either Rule String]) -> ([Rule],[String])  |   (cf,msgs1) = (CFG (exts,ruls2),msgs2)
  untag ls = ([c | Left c <- ls], [r| Right r <- ls])   |   (ruls2,msgs2) = untag $ partition (isRule) $ map (che
  -- isRule = either (const True) (const False)         |   untag (ls,rs) = ([c | Left c <- ls], [c | Right c <- 
  cf0 :: CF                                             |   isRule = either (const True) (const False)
  (cf0@(exts,_),msgs) = (revs . srt . conv $ g)         |   cf00 = cfp2cf cf0
  srt :: [Either (Either Pragma TempRule) String] -> (C |   (cf0@(CFG(exts,_)),msgs) = (revs . srt . conv . pGram
  srt rs = let                                          |   srt rs = let rules              = [r | Left (Right r)
       rules              = [fixRuleTokens n r | (n,Lef |                literals           = nub  [lit | xs <- m
       literals           = nub  [lit | Left xs <- map  |                                                 Left li
                                   (Left lit) <- xs,    <
                                   elem lit specialCats (
                                                        <
       pragma             = [r | Left (Left r) <- rs]   (
       tokens             = [i | TokenReg i _ _ <- prag <
       errors             = [s | Right s <- rs, not (nu (
       (symbols,keywords) = partition notIdent reserved (
       notIdent s         = null s || not (isIdentAlpha |                notIdent s         = null s || not (isAl
       isIdentAlpha c     = isLatin1 c && isAlpha c     |                isIdentRest c      = isAlphaNum c || c =
       isIdentRest c      = isIdentAlpha c || isDigit c |                reservedWords      = nub [t | r <- rules
       reservedWords      = nub [t | (_,(_,Left its)) < <
         concatMap (reservedLiteralAQ [ (b,i,a) | AntiQ <
       cats               = []                          (
            in (((pragma,(literals,symbols,keywords,cat |             in (CFG((pragma,(literals,symbols,keywords,
                                                        |   revs (cf@(CFG((pragma,(literals,symbols,keywords,_)),
  revs :: (CF, [String]) -> (CF, [String])              |     (CFG((pragma,
  revs (cf@((pragma,(literals,symbols,keywords,_)),rule |        (literals,symbols,keywords,findAllReversibleCats
    (((pragma,                                          <
       (literals,symbols,keywords,findAllReversibleCats <
                                                        <
fixRuleTokens :: Int -> TempRule -> Rule                <
fixRuleTokens n (f,(c,rhs)) =                           <
  (f,(c,either Left (\r -> Right (r,"RTL_"++show n)) rh <
                                                        (
                                                        | conv :: Err Abs.Grammar -> [Either (Either Pragma RuleP
                                                        <
                                                        <
                                                        <
conv :: ParseMonad Abs.Grammar -> [Either (Either Pragm <
conv (Bad s)                 = [Right s]                (
conv (Ok (Abs.Grammar defs)) = map Left $ concatMap (tr | conv (Ok (Abs.Grammar defs)) = map Left $ concatMap tra
                                                        (
reservedLiteralAQ []        l = []                      | transDef :: Abs.Def -> [Either Pragma RuleP]
reservedLiteralAQ [(b,i,a)] l = [b ++ l]                | transDef x = case x of
reservedLiteralAQ _         l = error "multiple antiquo <
                                                        <
isAqLabel x = case x of                                 <
  (Abs.Aq s)    -> True                                 <
--  Abs.LabP Abs.Aq _    -> True                        <
--  Abs.LabPF Abs.Aq _ _ -> True                        <
--  Abs.LabF Abs.Aq _    -> True                        <
--  _                    -> False                       <
                                                        <
transDef :: [Abs.Def] -> Abs.Def -> [Either Pragma Temp <
transDef defs x = case x of                             <
-- Abs.Rule label cat items | isAqLabel label -> []     <
 Abs.Rule label cat items ->                            (
   [Right (transLabel label,(transCat cat, transRHS ite |    [Right $ Rule (transLabel label,(transCat cat,map tr
 Abs.Comment str               -> [Left $ CommentS str] (
 Abs.Comments str0 str         -> [Left $ CommentM (str (
 Abs.Token ident reg           -> [Left $ TokenReg (tra (
 Abs.PosToken ident reg        -> [Left $ TokenReg (tra (
 Abs.Entryp idents             -> [Left $ EntryPoints ( (
 Abs.Internal label cat items  ->                       (
   [Right (transLabel label,(transCat cat,(Left $ Left  |    [Right $ Rule (transLabel label,(transCat cat,(Left 
 Abs.Separator size ident str -> map  Right $ separator |  Abs.Separator size ident str -> map  (Right . cf2cfpRu
 Abs.Terminator size ident str -> map  Right $ terminat |  Abs.Terminator size ident str -> map  (Right . cf2cfpR
 Abs.Coercions ident int -> map  (Right) $ coercionRule |  Abs.Coercions ident int -> map  (Right . cf2cfpRule) $
 Abs.Rules ident strs -> map (Right) $ ebnfRules ident  |  Abs.Rules ident strs -> map (Right . cf2cfpRule) $ ebn
 Abs.Layout ss      -> [Left $ Layout ss]               (
 Abs.LayoutStop ss  -> [Left $ LayoutStop ss]           (
 Abs.LayoutTop      -> [Left $ LayoutTop]               (
 Abs.Derive ss      -> [Left $ Derive [s|Abs.Ident s<-s |  Abs.Function f xs e -> [Left $ FunDef (transIdent f) (
-- Abs.Function f xs e -> [Left $ FunDef (transIdent f) <
 Abs.AntiQuote b i a ->                                 <
   [Left $ AntiQuote b i a]                             <
   ++ [Left  $ TokenReg "AqToken" False $ aqToken i a]  <
   ++ aqRules (b,i,a) (getCats defs) where              <
   reg = aqToken a                                      <
                                                        <
aqToken :: String -> String -> Abs.Reg                  <
aqToken i s@(c:cs) = Abs.RSeq (Abs.RSeqs i) $ Abs.RSeq  <
  prefixes = scanr (:) [c] . reverse $ cs               <
  clause (d:ds) = subclause (reverse ds) (Abs.RMinus Ab <
  subclause [] x = x                                    <
  subclause (e:es) x = Abs.RSeq (Abs.RChar e) (subclaus <
                                                        <
getCats :: [Abs.Def] -> [Cat]                           <
getCats = nub . concatMap (\x -> case x of              <
  Abs.Rule _ cat _     -> [transCat cat]                <
  Abs.Internal _ cat _ -> [transCat cat]                <
  _                    -> [])                           <
                                                        <
aqRHS :: [Abs.Item] -> Cat                              <
aqRHS xs = case filter filt xs of                       <
  [Abs.NTerminal cat] -> transCat cat                   <
  _ -> error "anti-quotation rules must have exactly on <
  where                                                 <
    filt x  =case x of                                  <
      Abs.Terminal str   -> False                       <
      Abs.NTerminal cat  -> True                        <
                                                        <
                                                        <
toks x = case x of                                      <
 Abs.Token (Abs.Ident ident) reg           -> [ident]   <
 Abs.PosToken (Abs.Ident ident) reg        -> [ident]   <
 _                                         -> []        <
                                                        <
aqRules :: (String,String,String) -> [String] -> [Eithe <
aqRules (b,i,a) = concatMap aqRule where                <
  aqRule cat = map Right [                              <
      (aqFun,(cat, Left [Right b,Left "AqToken"])),     <
      (aqFun,(cat, Left [Right (b++normCat cat), Left " <
      ]                                                 <
                                                        (
aqFun = "$global_aq"                                    | separatorRules :: Abs.MinimumSize -> Abs.Cat -> String 
                                                        <
-- addSpecials :: (String,String,String) -> [Either Pra <
-- addSpecials (b,i,a) rs = rs ++ concatMap special lit <
                                                        <
--  special aqs@('A':'Q':'_':s) = map Right [(aqs,(aqs, <
--    (renameAq s,(rename s, [Right b,Left "AqToken"])) <
--    (renameAqt s,(rename s, [Right (b++s), Left "AqTo <
--    ]                                                 <
--  rules              = [r | (Right r) <- rs]          <
--  literals           = nub  [lit | xs <- map (snd . s <
--    (Left lit) <- xs,                                 <
--    elem lit (map rename specialCatsP)]               <
-- \end{hack}                                           <
                                                        <
                                                        <
                                                        <
separatorRules :: Abs.MinimumSize -> Abs.Cat -> String  <
separatorRules size c s = if null s then terminatorRule (
  ("(:[])", (cs,Left [Left c'])),                       |   Rule ("(:[])", (cs,[Left c'])),
  ("(:)",   (cs,Left [Left c', Right s, Left cs]))      |   Rule ("(:)",   (cs,[Left c', Right s, Left cs]))
  ]                                                     (
 where                                                  (
   c' = transCat c                                      (
   cs = "[" ++ c' ++ "]"                                (
   ifEmpty rs = if (size == Abs.MNonempty) then rs else |    ifEmpty rs = if (size == Abs.MNonempty)
                                                        >                 then rs
                                                        >                 else (Rule ("[]", (cs,[])) : rs)
                                                        (
terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String | terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String
terminatorRules size c s = [                            (
  ifEmpty,                                              (
  ("(:)",   (cs,Left $ Left c' : s' [Left cs]))         |   Rule ("(:)",   (cs,Left c' : s' [Left cs]))
  ]                                                     (
 where                                                  (
   c' = transCat c                                      (
   cs = "[" ++ c' ++ "]"                                (
   s' its = if null s then its else (Right s : its)     (
   ifEmpty = if (size == Abs.MNonempty)                 (
                then ("(:[])",(cs,Left $ [Left c'] ++ i |                 then Rule ("(:[])",(cs,[Left c'] ++ if 
                else ("[]",   (cs,Left []))             |                 else Rule ("[]",   (cs,[]))
                                                        (
coercionRules :: Abs.Ident -> Integer -> [TempRule]     | coercionRules :: Abs.Ident -> Integer -> [Rule]
coercionRules (Abs.Ident c) n =                         (
   ("_", (c,               Left [Left (c ++ "1")])) :   |    Rule ("_", (c,               [Left (c ++ "1")])) :
  [("_", (c ++ show (i-1), Left [Left (c ++ show i)]))  |   [Rule ("_", (c ++ show (i-1), [Left (c ++ show i)])) 
  [("_", (c ++ show n,     Left [Right "(", Left c, Rig |   [Rule ("_", (c ++ show n,     [Right "(", Left c, Rig
                                                        <
                                                        (
ebnfRules :: Abs.Ident -> [Abs.RHS] -> [TempRule]       | ebnfRules :: Abs.Ident -> [Abs.RHS] -> [Rule]
ebnfRules (Abs.Ident c) rhss =                          (
  [(mkFun k c rhs, (c, transRHS rhs)) | (k, rhs) <- zip |   [Rule (mkFun k c its, (c, map transItem its))
                                                        >      | (k, Abs.RHS its) <- zip [1 :: Int ..] rhss]
 where                                                  (
   mkFun :: Int -> String -> Abs.RHS -> String          <
   mkFun k c i = case i of                              (
     (Abs.RHS [Abs.Terminal s])  -> c' ++ "_" ++ mkName |      [Abs.Terminal s]  -> c' ++ "_" ++ mkName k s
     (Abs.RHS [Abs.NTerminal n]) -> c' ++ identCat (tra |      [Abs.NTerminal n] -> c' ++ identCat (transCat n)
     _ -> c' ++ "_" ++ show k                           (
   c' = c --- normCat c                                 (
   mkName k s = if all (\c -> isAlphaNum c || elem c "_ (
                   then s else show k                   (
                                                        (
                                                        <
transRHS :: Abs.RHS -> TempRHS                          <
transRHS (Abs.RHS its) = Left $ map transItem its       <
transRHS (Abs.TRHS r)  = Right r                        <
                                                        <
                                                        <
                                                        <
transItem :: Abs.Item -> Either Cat String              (
transItem x = case x of                                 (
 Abs.Terminal str   -> Right str                        (
 Abs.NTerminal cat  -> Left (transCat cat)              (
                                                        (
transCat :: Abs.Cat -> Cat                              (
transCat x = case x of                                  (
 Abs.ListCat cat  -> "[" ++ (transCat cat) ++ "]"       (
 Abs.IdCat id     -> transIdent id                      (
                                                        (
transLabel :: Abs.Label -> Fun                          | transLabel :: Abs.Label -> (Fun,Prof)
transLabel y = let g = transLabelId y in g              | transLabel y = case y of
                                                        >    Abs.LabNoP f     -> let g = transLabelId f in (g,(g,
                                                        >    Abs.LabP   f p   -> let g = transLabelId f in (g,(g,
                                                        >    Abs.LabPF  f g p -> (transLabelId f,(transLabelId g,
                                                        >    Abs.LabF   f g   -> (transLabelId f,(transLabelId g,
 where                                                  (
   transLabelId x = case x of                           (
     Abs.Id id     -> transIdent id                     (
     Abs.Wild      -> "_"                               (
     Abs.ListE     -> "[]"                              (
     Abs.ListCons  -> "(:)"                             (
     Abs.ListOne   -> "(:[])"                           (
     Abs.Aq (Abs.JIdent i)     -> "$" ++ transIdent i   |    transProf (Abs.ProfIt bss as) = 
     Abs.Aq _     -> "$"                                |      ([map fromInteger bs | Abs.Ints bs <- bss], map fr
--   transProf (Abs.ProfIt bss as) =                    <
--     ([map fromInteger bs | Abs.Ints bs <- bss], map  <
                                                        (
transIdent :: Abs.Ident -> String                       (
transIdent x = case x of                                (
 Abs.Ident str  -> str                                  (
                                                        (
transArg :: Abs.Arg -> String                           (
transArg (Abs.Arg x) = transIdent x                     (
                                                        (
transExp :: Abs.Exp -> Exp                              (
transExp e = case e of                                  (
    Abs.App x es    -> App (transIdent x) (map transExp (
    Abs.Var x       -> App (transIdent x) []            (
    Abs.Cons e1 e2  -> cons e1 (transExp e2)            (
    Abs.List es     -> foldr cons nil es                (
    Abs.LitInt x    -> LitInt x                         (
    Abs.LitDouble x -> LitDouble x                      (
    Abs.LitChar x   -> LitChar x                        (
    Abs.LitString x -> LitString x                      (
  where                                                 (
    cons e1 e2 = App "(:)" [transExp e1, e2]            (
    nil        = App "[]" []                            (
                                                        (
                                                        > -------------------------------------------------------
                                                        (
                                                        > --checkTokens :: CFG f -> [String]
                                                        > checkTokens cf =
                                                        >     if null ns
                                                        >     then []
                                                        >     else ["Warning : ", -- change to error in a future 
                                                        >           "  The following tokens accept the empty stri
                                                        >           "    "++unwords ns,
                                                        >           "  This is error-prone and will not be suppor
                                                        >   where
                                                        >     ns = map fst . filter (nullable.snd) $ tokenPragmas
                                                        (
                                                        > -- | Check if a regular expression is nullable (accepts
                                                        > nullable :: Abs.Reg -> Bool
                                                        > nullable r =
                                                        >     case r of
                                                        >       Abs.RSeq r1 r2   -> nullable r1 && nullable r2
                                                        >       Abs.RAlt r1 r2   -> nullable r1 || nullable r2
                                                        >       Abs.RMinus r1 r2 -> nullable r1 && not (nullable 
                                                        >       Abs.RStar _      -> True
                                                        >       Abs.RPlus r1     -> nullable r1
                                                        >       Abs.ROpt _       -> True
                                                        >       Abs.REps         -> True
                                                        >       Abs.RChar _      -> False
                                                        >       Abs.RAlts _      -> False
                                                        >       Abs.RSeqs s      -> null s
                                                        >       Abs.RDigit       -> False
                                                        >       Abs.RLetter      -> False
                                                        >       Abs.RUpper       -> False
                                                        >       Abs.RLower       -> False
                                                        >       Abs.RAny         -> False

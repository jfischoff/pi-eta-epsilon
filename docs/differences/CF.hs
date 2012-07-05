{-#Language PatternGuards#-}                            | {-# LANGUAGE PatternGuards #-}
{-                                                      (
    BNF Converter: Abstract syntax                      (
    Copyright (C) 2004  Author:  Markus Forberg, Michae (
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
module Language.LBNF.CF (                               | module CF (
            -- Types.                                   (
            CF,                                         (
            RHS,                                        |             CFG(..), pragmasOfCF, -- ...
            Rule, funRule, isTokenRule,                 |             Rule, Rul(..), lookupRule,
            Pragma(..),                                 (
            Reg(..),                                    <
            Exp(..),                                    (
            Literal,                                    (
            Symbol,                                     (
            KeyWord,                                    (
            Cat,                                        (
            Fun,                                        (
            Tree(..),                                   (
            prTree,         -- print an abstract syntax (
            Data,           -- describes the abstract s (
            cf2data,        -- translates a grammar to  (
            -- cf2dataLists,   -- translates to a Data  |             cf2dataLists,   -- translates to a Data wit
            -- Literal categories, constants,           (
            firstCat,       -- the first value category (
            firstEntry,     -- the first entry or the f (
            specialCats,    -- ident                    (
            specialCatsP,   -- all literals             (
            specialData,    -- special data             (
            isCoercion,     -- wildcards in grammar (av (
            isDefinedRule,  -- defined rules (allows sy (
            isProperLabel,  -- not coercion or defined  (
            allCats,        -- all categories of a gram (
            allCatsIdNorm,                              (
            allEntryPoints, -- those categories that ar (
            reservedWords,  -- get the keywords of a gr (
            symbols,        -- get all symbols          (
            literals,       -- get all literals of a gr (
            typed_literals,                             <
            reversibleCats, -- categories that is left- (
            findAllReversibleCats, -- find all reversib (
            identCat,       -- transforms '[C]' to List (
            valCat,         -- The value category of a  (
            isParsable,     -- Checks if the rule is pa (
            rulesOfCF,      -- All rules of a grammar.  (
            rulesForCat,    -- rules for a given catego (
            ruleGroups,     -- Categories are grouped w (
            ruleGroupsInternals, --As above, but includ (
            notUniqueFuns,   -- Returns a list of funct |             funRule,        -- The function name of a r
            badInheritence, -- Returns a list of all fu |             notUniqueNames, -- list of not unique names
                                                        > --          notUniqueFuns,   -- Returns a list of funct
                                                        > --            badInheritence, -- Returns a list of all 
            isList,         -- Checks if a category is  (
            -- Information functions for list functions (
            isNilFun,       -- empty list function? ([] (
            isOneFun,       -- one element list functio (
            isConsFun,      -- constructor function? (: (
            isNilCons,      -- either three of above?   (
            isEmptyListCat, -- checks if the list permi (
            revSepListRule, -- reverse a rule, if it is (
            rhsRule,        -- The list of Terminals/No (
            normCat,        -- Removes precendence info (
            normCatOfList,  --   Removes precendence in (
            catOfList,      -- Removes enclosed list: [ (
            comments,       -- translates the pragmas i (
            ruleTokens,                                 <
            tokenPragmas,   -- user-defined regular exp (
            tokenNames,     -- The names of all user-de (
            precCat,        -- get the precendence leve (
            precLevels,     -- get all precendence leve (
            precRule,       -- get the precendence leve (
            precCF,         -- Check if the CF consists (
            isUsedCat,                                  (
            internalCat,    -- the symbol #             (
            isPositionCat,  -- category that has a posi (
            isNormal,                                   <
            isAqFun,                                    <
            hasIdent,                                   (
            hasLayout,                                  (
            hasAq,                                      <
            rename,                                     <
            renameAq,                                   <
            renameAqt,                                  <
            unAq,                                       <
            unAqs,                                      <
            aqSyntax,                                   <
            -- resolveAq,                               <
            layoutPragmas,                              (
            derivations,                                <
            checkRule,                                  (
            visibleNames,                               |
            quoterName,                                 <
            quoters,                                    <
{-                                                      <
            CFP,            -- CF with profiles         (
            RuleP,                                      (
            FunP,                                       (
            Prof,                                       (
            cf2cfpRule,                                 (
            cf2cfp,                                     (
            cfp2cf,                                     (
            trivialProf,                                (
            rulesOfCFP,                                 (
            funRuleP, ruleGroupsP, allCatsP, allEntryPo (
-}                                                      <
           ) where                                      (
                                                        (
import Language.LBNF.Utils (prParenth,(+++))            | import Utils (prParenth,(+++))
import Data.List (nub, intersperse, partition, sort,sor (
import Data.Char                                        (
import Language.LBNF.Grammar (Reg())                    | import AbsBNF (Reg())
                                                        <
                                                        (
-- A context free grammar consists of a set of rules an (
-- information (e.g. pragmas, literals, symbols, keywor (
-- data CF = MkCF {                                     | type CF = CFG Fun -- (Exts,[Rule])
--   rulesOfCF :: CF -> [Rule]                          <
-- , infoOfCF :: CFG f -> Info                          <
-- , pragmasOfCF :: CFG f -> [Pragma]                   <
-- }                                                    <
type CF = (Exts,[Rule])                                 <
                                                        <
rulesOfCF :: CF -> [Rule]                               <
rulesOfCF = snd                                         <
                                                        <
infoOfCF :: CFG f -> Info                               <
infoOfCF = snd . fst                                    <
                                                        <
pragmasOfCF :: CFG f -> [Pragma]                        <
pragmasOfCF = fst . fst                                 <
                                                        <
                                                        (
-- A rule consists of a function name, a main category  (
-- terminals and non-terminals.                         (
-- function_name . Main_Cat ::= sequence                (
                                                        | type Rule = Rul Fun -- (Fun, (Cat, [Either Cat String])
isTokenRule :: Rule -> Bool                             <
isTokenRule = either (const False) (const True) . rhsRu <
                                                        <
funRule :: Rule -> Fun                                  <
funRule = fst                                           <
                                                        <
oldRHS = either id (const [])                           <
                                                        <
rhsRule :: Rule -> RHS                                  <
rhsRule = snd . snd                                     <
                                                        <
                                                        <
type RHS  = Either [Either Cat String] (Reg,String)     <
type Rule = (Fun, (Cat, RHS))                           <
                                                        (
-- polymorphic types for common type signatures for CF  (
type Rul f = Rule -- (f, (Cat, [Either Cat String]))    | newtype Rul f = Rule { unRule::(f, (Cat, [Either Cat St
type CFG f = CF -- (Exts,[Rul f])                       |                 deriving Eq
                                                        > newtype CFG f = CFG { unCFG :: (Exts,[Rul f]) }
                                                        (
type Exts = ([Pragma],Info)                             (
-- Info is information extracted from the CF, for easy  (
-- Literals - Char, String, Ident, Integer, Double      (
--            Strings are quoted strings, and Ident are (
-- Symbols  - symbols in the grammar, e.g. '*', '->'.   | -- Symbols  - symbols in the grammar, e.g. ´*´, '->'.
-- KeyWord  - reserved words, e.g. 'if' 'while'         (
type Info = ([Literal],[Symbol],[KeyWord],[Cat])        (
                                                        (
-- Expressions for function definitions                 (
data Exp = App String [Exp]                             (
         | LitInt Integer                               (
         | LitDouble Double                             (
         | LitChar Char                                 (
         | LitString String                             (
           deriving (Eq)                                <
                                                        (
instance Show Exp where                                 (
    showsPrec p e =                                     (
        case listView e of                              (
            Right es    ->                              (
                showString "["                          (
                . foldr (.) id (intersperse (showString (
                . showString "]"                        (
            Left (App x []) -> showString x             (
            Left (App "(:)" [e1,e2]) ->                 (
                showParen (p>0)                         (
                $ showsPrec 1 e1                        (
                . showString " : "                      (
                . shows e2                              (
            Left (App x es) ->                          (
                showParen (p>1)                         (
                $ foldr (.) id                          (
                $ intersperse (showString " ")          (
                $ showString x : map (showsPrec 2) es   (
            Left (LitInt n)     -> shows n              (
            Left (LitDouble x)  -> shows x              (
            Left (LitChar c)    -> shows c              (
            Left (LitString s)  -> shows s              (
        where                                           (
            listView (App "[]" []) = Right []           (
            listView (App "(:)" [e1,e2])                (
                | Right es <- listView e2   = Right $ e (
            listView e  = Left e                        (
                                                        (
-- pragmas for single line comments and for multiple-li (
data Pragma = CommentS  String                          (
            | CommentM (String,String)                  (
            | TokenReg String Bool Reg                  (
            | EntryPoints [Cat]                         (
            | Layout [String]                           (
            | LayoutStop [String]                       (
            | LayoutTop                                 (
            | Derive [String]                           <
            | FunDef String [String] Exp                (
            | AntiQuote String String String            <
            -- ...                                      (
              deriving (Show, Eq)                       |               deriving (Show)
                                                        (
ruleTokens :: CF -> [(String,Reg)]                      | tokenPragmas :: CFG f -> [(String,Reg)]
ruleTokens cf = [(token,reg) | (fun,(c,Right (reg,token <
                                                        <
tokenPragmas :: CF -> [(String,Reg)]                    <
tokenPragmas cf = [(name,exp) | TokenReg name _ exp <-  (
                                                        (
tokenNames :: CF -> [String]                            (
tokenNames cf = fst (unzip (tokenPragmas cf))           (
                                                        (
layoutPragmas :: CF -> (Bool,[String],[String])         (
layoutPragmas cf = let ps = pragmasOfCF cf in (         (
  not (null [() | LayoutTop  <- ps]),   -- if there's l (
  concat [ss | Layout ss     <- ps],    -- layout-block (
  concat [ss | LayoutStop ss <- ps]     -- layout-block (
  )                                                     (
                                                        (
derivations :: CF -> [String]                           <
derivations cf  = case concat [ss|Derive ss <- pragmasO <
  [] -> ["Show", "Eq", "Ord"]                           <
  x  -> x                                               <
                                                        <
                                                        <
hasLayout :: CF -> Bool                                 (
hasLayout cf = case layoutPragmas cf of                 (
  (t,ws,_) -> t || not (null ws)   -- (True,[],_) means (
                                                        (
hasAq :: CF -> Bool                                     <
hasAq cf = case [(b,a) | AntiQuote b i a <- pragmasOfCF <
  [] -> False                                           <
  _  -> True                                            <
                                                        <
aqSyntax :: CF -> Maybe (String,String,String)          <
aqSyntax cf = case [(b,i,a) | AntiQuote b i a <- pragma <
  [] -> Nothing                                         <
  [t] -> Just t                                         <
  many -> error "aqSyntax: Multiple antiquote pragmas"  <
{-                                                      <
resolveAq cf@((ps,(i,t,y,z)),rs0) = maybe cf addAqRules <
  addAqRules (b,a) = ((map renamePragma ps,(newi,nub $  <
    rs = map renameRule (rulesOfCF cf) ++ newRules ++ c <
    newi = nub $ "String":i                             <
    newRules = map mkNewRule $ filter isNormal $ allCat <
    mkNewRule s = (renameAq s,(rename s,map Right b ++  <
    renameRule (fun,(cat,itms)) = (rename fun,(rename c <
    renameItem = either (Left . rename) (Right . id)    <
                                                        <
    renamePragma p = case p of                          <
      EntryPoints cs -> EntryPoints $ map rename cs     <
      _   -> p                                          <
    newType s = [                                       <
      (rename s,(rename s,[Left $ s])),                 <
      (s++"__AQ",(rename s,map Right b ++ [Left $ "Stri <
      ]                                                 <
-}                                                      <
                                                        <
rename s = case s of                                    <
    "_" -> s                                            <
    "$" -> s                                            <
    "#" -> s                                            <
    "(:)" -> s                                          <
    "(:[])" -> s                                        <
    "[]" -> s                                           <
    ('$':s) -> "AQ___" ++ s                             <
    ('[':l) -> '[' : rename (init l) ++ "]"             <
    _ -> "AQ_" ++ normCat s ++ number s                 <
                                                        <
renameAqt s = case s of                                 <
    ('[':l) -> '[' : renameAqt (init l) ++ "]"          <
    _ -> "AQ___" ++ normCat s ++ number s               <
                                                        <
renameAq s = case s of                                  <
    ('[':l) -> '[' : renameAq (init l) ++ "]"           <
    _ -> "AQ__" ++ normCat s ++ number s                <
                                                        <
number = reverse . takeWhile isDigit . reverse          <
                                                        <
unAq s = case s of                                      <
  'A':'Q':'_':r -> Just r                               <
  _             -> Nothing                              <
                                                        <
unAqs s = case s of                                     <
  'A':'Q':'_':'_':'_':r -> Just r                       <
  'A':'Q':'_':'_':r -> Just r                           <
  _             -> Nothing                              <
                                                        <
-- Literal: Char, String, Ident, Integer, Double        (
type Literal = Cat                                      (
type Symbol  = String                                   (
type KeyWord = String                                   (
                                                        (
-- Cat is the Non-terminals of the grammar.             (
type Cat     = String                                   (
-- Fun is the function name of a rule.                  (
type Fun     = String                                   (
                                                        > -- both cat and fun
                                                        > type Name = String
                                                        (
internalCat :: Cat                                      (
internalCat = "#"                                       (
                                                        (
-- Abstract syntax tree.                                (
newtype Tree = Tree (Fun,[Tree])                        (
                                                        (
-- The abstract syntax of a grammar.                    (
type Data = (Cat, [(Fun,Either [Cat] String)])          | type Data = (Cat, [(Fun,[Cat])])
                                                        (
-- firstCat returns the first Category appearing in the (
firstCat :: CF -> Cat                                   (
firstCat = valCat . head . rulesOfCF                    (
                                                        (
firstEntry :: CF -> Cat                                 (
firstEntry cf = case allEntryPoints cf of               (
                 (x:_) -> x                             (
                 _     -> firstCat cf                   (
                                                        (
                                                        > rulesOfCF   :: CF -> [Rule]
                                                        > rulesOfCFP  :: CFP -> [RuleP]
                                                        > infoOfCF    :: CFG f -> Info
                                                        > pragmasOfCF :: CFG f -> [Pragma]
                                                        >
                                                        > rulesOfCF   = snd . unCFG
                                                        > rulesOfCFP  = snd . unCFG
                                                        > infoOfCF    = snd . fst . unCFG
                                                        > pragmasOfCF = fst . fst . unCFG
                                                        >
                                                        > -- aggressively ban nonunique names (AR 31/5/2012)
                                                        >
                                                        > notUniqueNames :: [Name] -> CF -> [Fun]
                                                        > notUniqueNames reserved cf = [head xs | xs <- xss, leng
                                                        >   xss = group (sort names)
                                                        >   names = reserved ++ allCatsIdNorm cf ++ allFuns cf
                                                        >   allFuns g = [ f | f <- map funRule (rulesOfCF g), not
                                                        (
                                                        > -- obsolete:
                                                        (
notUniqueFuns :: CF -> [Fun]                            (
notUniqueFuns cf = let xss = group $ sort [ f | (f,_) < | notUniqueFuns cf = let xss = group $ sort [ f | f <- ma
                                                 not (i (
                    in [ head xs | xs <- xss, length xs (
                                                        (
badInheritence :: CF -> [Cat]                           (
badInheritence cf = concatMap checkGroup (ruleGroups cf (
 where                                                  (
  checkGroup (cat, rs) = if (length rs <= 1)            (
                           then []                      (
                           else case lookup cat rs of   |                            else case lookupRule cat rs 
                             Nothing -> []              (
                             Just x -> [cat]            (
                                                        (
                                                        <
                                                        <
-- extract the comment pragmas.                         (
commentPragmas :: [Pragma] -> [Pragma]                  (
commentPragmas = filter isComment                       (
 where isComment (CommentS _) = True                    (
       isComment (CommentM _) = True                    (
       isComment _            = False                   (
                                                        (
                                                        > lookupRule :: Eq f => f -> [Rul f] -> Maybe (Cat, [Eith
                                                        > lookupRule f = lookup f . map unRule
                                                        >
-- returns all normal rules that constructs the given C (
rulesForCat :: CF -> Cat -> [Rule]                      (
rulesForCat cf cat = [normRuleFun r | r <- rulesOfCF cf (
                                                        (
--This version doesn't exclude internal rules.          (
rulesForCat' :: CF -> Cat -> [Rule]                     (
rulesForCat' cf cat = [normRuleFun r | r <- rulesOfCF c (
                                                        (
valCat :: Rul f -> Cat                                  (
valCat = fst . snd                                      | valCat = fst . snd . unRule
                                                        (
-- Get all categories of a grammar.                     (
allCats :: CF -> [Cat]                                  (
allCats = nub . map valCat . rulesOfCF -- no cats w/o p (
                                                        (
-- Gets all normalized identified Categories            (
allCatsIdNorm :: CF -> [Cat]                            (
allCatsIdNorm = nub . map identCat . map normCat . allC (
                                                        (
-- category is used on an rhs                           (
isUsedCat :: CF -> Cat -> Bool                          (
isUsedCat cf cat = elem cat [c | r <- (rulesOfCF cf), L | isUsedCat cf cat = elem cat [c | r <- (rulesOfCF cf), L
                                                        (
-- entry points to parser ----                          (
allEntryPoints :: CF -> [Cat]                           (
allEntryPoints cf = case concat [cats | EntryPoints cat (
  [] -> allCats cf                                      (
  cs -> cs                                              (
                                                        (
-- group all categories with their rules.               (
ruleGroups :: CF -> [(Cat,[Rule])]                      (
ruleGroups cf = [(c, rulesForCat cf c) | c <- allCats c (
                                                        (
-- group all categories with their rules including inte (
ruleGroupsInternals :: CF -> [(Cat,[Rule])]             (
ruleGroupsInternals cf = [(c, rulesForCat' cf c) | c <- (
                                                        (
typed_literals :: CF -> [(Fun,Cat)]                     | literals :: CFG f -> [Cat]
typed_literals cf = map (\x -> (x,x)) lits ++ owns      <
 where                                                  <
   (lits,_,_,_) = infoOfCF cf                           <
   owns = map (\(x,_) -> (x,x)) (tokenPragmas cf) -- ++ <
   rulets = [(fun,c) | (fun,(c,Right reg)) <- rulesOfCF <
                                                        <
literals :: CF -> [Cat]                                 <
literals cf = lits ++ owns                              (
 where                                                  (
   (lits,_,_,_) = infoOfCF cf                           (
   owns = map fst $ tokenPragmas cf ++ ruleTokens cf    |    owns = map fst $ tokenPragmas cf
                                                        (
symbols :: CFG f -> [String]                            (
symbols cf = syms                                       (
 where (_,syms,_,_) = infoOfCF cf                       (
                                                        (
reservedWords :: CFG f -> [String]                      (
reservedWords cf = sort keywords                        (
 where (_,_,keywords,_) = infoOfCF cf                   (
                                                        (
reversibleCats :: CFG f -> [Cat]                        (
reversibleCats cf = cats                                (
  where (_,_,_,cats) = infoOfCF cf                      (
                                                        (
-- Comments can be defined by the 'comment' pragma      (
comments :: CF -> ([(String,String)],[String])          (
comments cf = case commentPragmas (pragmasOfCF cf) of   (
               xs -> ([p | CommentM p <- xs],           (
                      [s | CommentS s <- xs])           (
                                                        (
                                                        > funRule :: Rule -> Fun
                                                        > funRule = fst . unRule
                                                        (
                                                        | rhsRule :: Rul f -> [Either Cat String]
                                                        > rhsRule = snd . snd . unRule
                                                        (
-- built-in categories (corresponds to lexer)           (
                                                        (
-- if the gramamr uses the predefined Ident type        (
hasIdent :: CF -> Bool                                  (
hasIdent cf = isUsedCat cf "Ident"                      (
                                                        (
-- these need new datatypes                             (
specialCats :: CF -> [Cat]                              (
specialCats cf = (if hasIdent cf then ("Ident":) else i (
                                                        (
-- the parser needs these                               (
specialCatsP :: [Cat]                                   (
specialCatsP = words "Ident Integer String Char Double" (
                                                        (
-- to print parse trees                                 (
prTree :: Tree -> String                                (
prTree (Tree (fun,[])) = fun                            (
prTree (Tree (fun,trees)) = fun +++ unwords (map pr2 tr (
  pr2 t@(Tree (_,ts)) = (if (null ts) then id else prPa (
                                                        (
-- abstract syntax trees: data type definitions         (
                                                        (
cf2data :: CF -> [Data]                                 (
cf2data cf =                                            (
  [(cat, nub (map mkData [r | r@(f,_) <- rulesOfCF cf,  |   [(cat, nub (map mkData [r | r <- rulesOfCF cf,
                                                        >                               let f=funRule r,
                              not (isDefinedRule f),    (
                              not (isCoercion f), eqCat |                               not (isCoercion f), eqCat
                              not (isAqFun f)]))        <
      | cat <- allNormalCats cf]                        (
 where                                                  (
  mkData :: Rule -> (Fun,Either [Cat] (String))         |   mkData (Rule (f,(_,its))) = (normFun f,[normCat c | L
  mkData (f,(_,Left its)) = (normFun f,Left [normCat c  <
  mkData (f,(_,Right (r,tok)))  = (normFun f,Right tok) <
                                                        (
{-                                                      <
--This version includes lists in the returned data.     (
--Michael 4/03                                          (
cf2dataLists :: CF -> [Data]                            (
cf2dataLists cf =                                       (
  [(cat, nub (map mkData [r | r@(f,_) <- rulesOfCF cf,  |   [(cat, nub (map mkData [r | r <- rulesOfCF cf,
                                                        >                               let f = funRule r,
                              not (isDefinedRule f),    (
                              not (isCoercion f), eqCat (
      | cat <- (filter (\x -> not $ isDigit $ last x) ( (
 where                                                  (
  mkData (f,(_,its)) = (normFun f,[normCat c | Left c < |   mkData (Rule (f,(_,its))) = (normFun f,[normCat c | L
-}                                                      <
                                                        (
specialData :: CF -> [Data]                             (
specialData cf = [(c,[(c,Left [arg c])]) | c <- special | specialData cf = [(c,[(c,[arg c])]) | c <- specialCats 
  arg c = case c of                                     (
    _ -> "String"                                       (
                                                        (
allNormalCats :: CF -> [Cat]                            (
allNormalCats = filter isNormal . allCats               (
                                                        (
-- to deal with coercions                               (
                                                        (
-- the Haskell convention: the wildcard _ is not a cons (
                                                        (
isCoercion :: Fun -> Bool                               (
isCoercion = (== "_")                                   (
                                                        (
isDefinedRule :: Fun -> Bool                            (
isDefinedRule (x:_) = isLower x                         (
                                                        (
isProperLabel :: Fun -> Bool                            (
isProperLabel f = not (isCoercion f || isDefinedRule f) (
                                                        (
-- categories C1, C2,... (one digit in end) are variant (
                                                        (
eqCat :: Cat -> Cat -> Bool                             (
eqCat c c1 = catCat c == catCat c1                      (
                                                        (
normCat :: Cat -> Cat                                   (
normCat c = case c of                                   (
  '[':cs -> "[" ++ norm (init cs) ++ "]"                (
  _     -> unList $ norm c -- to be deprecated          (
 where                                                  (
   norm = reverse . dropWhile isDigit . reverse         (
                                                        (
normCatOfList :: Cat -> Cat                             (
normCatOfList = normCat . catOfList                     (
                                                        (
-- for Happy and Latex                                  (
-- When given a list Cat, i.e. '[C]', it removes the sq (
-- and adds the prefix List, i.e. 'ListC'.              (
identCat :: Cat -> Cat                                  (
identCat c = case c of                                  (
  '[':cs -> "List" ++ identCat (init cs)                (
  _ -> c                                                (
                                                        (
normFun :: Fun -> Fun                                   (
normFun = id -- takeWhile (not . isDigit)               (
                                                        (
normRuleFun :: Rule -> Rule                             (
normRuleFun (f,p) = (normFun f, p)                      | normRuleFun (Rule (f,p)) = Rule (normFun f, p)
                                                        (
isNormal :: Cat -> Bool                                 (
isNormal c = not (isList c || isDigit (last c) || isAqF | isNormal c = not (isList c || isDigit (last c))
                                                        (
isParsable :: Rul f -> Bool                             (
isParsable (_,(_, Left (Left "#":_))) = False           | isParsable (Rule (_,(_, Left "#":_))) = False
isParsable (_,(_, Left (Left "$":_))) = False           <
isParsable _ = True                                     (
                                                        (
isList :: Cat -> Bool                                   (
isList c = head c == '['                                (
                                                        (
unList :: Cat -> Cat                                    (
unList c = c                                            (
                                                        (
catOfList :: Cat -> Cat                                 (
catOfList c = case c of                                 (
  '[':_:_ -> init (tail c)                              (
  _ -> c                                                (
                                                        (
isNilFun, isOneFun, isConsFun, isNilCons, isAqFun :: Fu | isNilFun, isOneFun, isConsFun, isNilCons :: Fun -> Bool
isNilCons f = isNilFun f || isOneFun f || isConsFun f   (
isNilFun f  = f == "[]"                                 (
isOneFun f  = f == "(:[])"                              (
isConsFun f = f == "(:)"                                (
                                                        (
isEmptyListCat :: CF -> Cat -> Bool                     (
isEmptyListCat cf c = elem "[]" $ map fst $ rulesForCat | isEmptyListCat cf c = elem "[]" $ map funRule $ rulesFo
                                                        (
isNonterm = either (const True) (const False)           (
                                                        (
isAqFun ('$':_) = True                                  <
isAqFun _       = False                                 <
                                                        <
-- used in Happy to parse lists of form 'C t [C]' in re (
-- applies only if the [] rule has no terminals         (
revSepListRule :: Rul f -> Rul f                        (
revSepListRule r@(f,(c, Left ts)) = (f, (c, Left $ xs : | revSepListRule (Rule (f,(c, ts))) = Rule (f, (c, xs : x
  (x,sep,xs) = (head ts, init (tail ts), last ts)       (
revSepListRule x = x                                    <
-- invariant: test in findAllReversibleCats have been p (
                                                        (
findAllReversibleCats :: CF -> [Cat]                    (
findAllReversibleCats cf = [c | (c,r) <- ruleGroups cf, (
  isRev c rs = case rs of                               (
     [r1,r2] | isList c -> if isConsFun (funRule r2)    (
                             then tryRev r2 r1          (
                           else if isConsFun (funRule r (
                             then tryRev r1 r2          (
                           else False                   (
     _ -> False                                         (
  tryRev :: Rule ->  Rule ->  Bool                      |   tryRev (Rule (f,(_,ts@(x:_:xs)))) r = isEmptyNilRule 
  tryRev (f,(_,Left (ts@(x:_:xs)))) r = isEmptyNilRule  <
                                 isConsFun f && isNonte (
  tryRev _ _ = False                                    (
                                                        (
isEmptyNilRule (f,(_,Left ts)) = isNilFun f && null ts  | isEmptyNilRule (Rule (f,(_,ts))) = isNilFun f && null t
isEmptyNilRule _ = False                                <
                                                        (
precCat :: Cat -> Int                                   (
precCat = snd . analyseCat                              (
                                                        (
precRule :: Rule -> Int                                 (
precRule = precCat . valCat                             (
                                                        (
precLevels :: CF -> [Int]                               (
precLevels cf = sort $ nub $ [ precCat c | c <- allCats (
                                                        (
precCF :: CF -> Bool                                    (
precCF cf = length (precLevels cf) > 1                  (
                                                        (
catCat :: Cat -> Cat                                    (
catCat = fst . analyseCat                               (
                                                        (
analyseCat :: Cat -> (Cat,Int)                          (
analyseCat c = if (isList c) then list c else noList c  (
 where                                                  (
  list   cat = let (rc,n) = noList (init (tail cat)) in (
  noList cat = case span isDigit (reverse cat) of       (
                ([],c') -> (reverse c', 0)              (
                (d,c') ->  (reverse c', read (reverse d (
                                                        (
-- we should actually check that                        (
-- (1) coercions are always between variants            (
-- (2) no other digits are used                         (
                                                        (
checkRule :: CF -> Rule -> Either Rule String           | checkRule :: CF -> RuleP -> Either RuleP String
checkRule cf r@(f,(cat,rhs))                            | checkRule cf r@(Rule((f,_),(cat,rhs)))
  | badCoercion    = Right $ "Bad coercion in rule" +++ (
  | badNil         = Right $ "Bad empty list rule" +++  (
  | badOne         = Right $ "Bad one-element list rule (
  | badCons        = Right $ "Bad list construction rul (
  | badList        = Right $ "Bad list formation rule"  (
  | badSpecial     = Right $ "Bad special category rule (
  | badTypeName    = Right $ "Bad type name" +++ unword (
  | badFunName     = Right $ "Bad constructor name" +++ (
  | badMissing     = Right $ "No production for" +++ un (
                             ", appearing in rule" +++  (
  | otherwise      = Left r                             (
 where                                                  (
   s  = f ++ "." +++ cat +++ "::=" +++ unwords (map (ei |    s  = f ++ "." +++ cat +++ "::=" +++ unwords (map (ei
   c  = normCat cat                                     (
   cs = [normCat c | Left c <- transRHS rhs]            |    cs = [normCat c | Left c <- rhs]
   badCoercion = isCoercion f && not ([c] == cs)        (
   badNil      = isNilFun f   && not (isList c && null  (
   badOne      = isOneFun f   && not (isList c && cs == (
   badCons     = isConsFun f  && not (isList c && cs == (
   badList     = isList c     &&                        (
                 not (isCoercion f || isNilFun f || isO |                  not (isCoercion f || isNilFun f || isO
   badSpecial  = elem c specialCatsP && not (isCoercion (
                                                        (
   badMissing  = not (null missing)                     (
   missing     = filter nodef [c | Left c <- transRHS r |    missing     = filter nodef [c | Left c <- rhs] 
   nodef t = notElem t defineds                         (
   defineds =                                           (
    "#" : map fst (tokenPragmas cf) ++ specialCatsP ++  (
   badTypeName = not (null badtypes)                    (
   badtypes = filter isBadType $ cat : [c | Left c <- t |    badtypes = filter isBadType $ cat : [c | Left c <- r
   isBadType c = not (isUpper (head c) || isList c || c (
   badFunName = not (all (\c -> isAlphaNum c || c == '_ (
       || isCoercion f || isNilFun f || isOneFun f || i |                        || isCoercion f || isNilFun f ||
   transRHS :: RHS -> [Either Cat String]               <
   transRHS = either id (const [])                      <
                                                        (
isPositionCat :: CFG f -> Cat -> Bool                   (
isPositionCat cf cat =  or [b | TokenReg name b _ <- pr (
                                                        (
                                                        (
visibleNames :: CF -> [String]                          | -- grammar with permutation profile à la GF. AR 22/9/200
visibleNames cf = "myLexer":"tokens":map ('p':) eps ++  |
  eps = quoters cf                                      | type CFP   = CFG FunP -- (Exts,[RuleP])
                                                        | type FunP  = (Fun,Prof)
quoterName :: String -> String                          | type RuleP = Rul FunP -- (FunP, (Cat, [Either Cat Strin
quoterName = initLower -- FIXME: List cats              |
                                                        | type Prof  = (Fun, [([[Int]],[Int])]) -- the original f
quoters :: CF -> [String]                               |
quoters = map identCat . allEntryPoints -- FIXME: List  | cf2cfp :: CF -> CFP
                                                        | cf2cfp (CFG (es,rs)) = CFG (es, map cf2cfpRule rs)
initLower :: String -> String                           |
initLower []  = error "initLower : Empty list"          | cf2cfpRule :: Rule -> RuleP
initLower (c:cs) = toLower c : cs                       | cf2cfpRule (Rule (f,(c,its)))  = Rule ((f, (f, trivialP
                                                        >
                                                        > cfp2cf :: CFP -> CF
                                                        > cfp2cf (CFG (es,rs)) = CFG (es,[Rule (f,(c,its)) | Rule
                                                        >
                                                        > trivialProf :: [Either Cat String] -> [([[Int]],[Int])]
                                                        > trivialProf its = [([],[i]) | (i,_) <- zip [0..] [c | L
                                                        >
                                                        > funRuleP :: RuleP -> Fun
                                                        > funRuleP = fst . snd . fst . unRule
                                                        >
                                                        > ruleGroupsP :: CFP -> [(Cat,[RuleP])]
                                                        > ruleGroupsP cf = [(c, rulesForCatP cf c) | c <- allCats
                                                        >
                                                        > rulesForCatP :: CFP -> Cat -> [RuleP]
                                                        > rulesForCatP cf cat = [r | r <- rulesOfCFP cf, isParsab
                                                        >
                                                        > allCatsP :: CFP -> [Cat]
                                                        > allCatsP = nub . map valCat . rulesOfCFP -- no cats w/o
                                                        >
                                                        > allEntryPointsP :: CFP -> [Cat]
                                                        > allEntryPointsP cf = case concat [cats | EntryPoints ca
                                                        >   [] -> allCatsP cf
                                                        >   cs -> cs

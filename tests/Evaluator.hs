{-# LANGUAGE TemplateHaskell    #-} 
{-# LANGUAGE QuasiQuotes        #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Evaluator where
import Language.PiEtaEpsilon.Grammar
import Language.PiEtaEpsilon.Types
import Language.PiEtaEpsilon.Parser
import Language.PiEtaEpsilon.Evaluator
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.Feat
import Data.Typeable
import Language.LBNF.Runtime
import Control.Applicative
import Data.Monoid
import Test.Framework.Options
import Test.HUnit as H

termEval :: UValue -> Term -> [UValue]
termEval i x = topLevel x i

isoEval :: UValue -> Iso -> [UValue]
isoEval i = termEval i . TBase

isoEval'  = isoEval  unit
termEval' = termEval unit

toUV = toP . toValueF

isoTest :: Iso -> Value -> Maybe Value -> Assertion
isoTest iso input output = case output of 
   Nothing -> assertBool (show input ++ " evaled to non null") . null $ 
               isoEval (toUV input) iso 
   Just o  -> case isoEval (toUV input) iso of
         []   -> assertFailure $ "empty list of results " ++ show input
         x:[] -> assertBool (show input ++ " doesn't eval to " ++ show output)
                  . closedAndEqual x . toUV $ o
         x:_  -> assertFailure $ 
                     "too many results when evaluating " ++ show input

termTest :: Term -> Value -> Maybe Value -> Assertion
termTest term input output = case output of 
   Nothing -> assertBool (show input ++ " evaled to non null") . null $ 
                termEval (toUV input) term  
   Just o  -> case termEval (toUV input) term of
         []   -> assertFailure $ "empty list of results " ++ show input
         x:[] -> assertBool (show input ++ " doesn't eval to " ++ show output)
                  . closedAndEqual x . toUV $ o
         x:_  -> assertFailure $ 
                     "too many results when evaluating " ++ show input

case_Eliminate_IdentityS         = 
   isoTest [iso| # <=+=> |] [value| R 1        |] $ Just [value| 1       |]
case_Introduce_IdentityS         =                
   isoTest [iso| ' <=+=> |] [value| 1          |] $ Just [value| R 1     |]
case_Eliminate_CommutativeS      =                
   isoTest [iso| #  x+x  |] [value| R 1        |] $ Just [value| L 1     |]
case_Introduce_CommutativeS      =                
   isoTest [iso| '  x+x  |] [value| R 1        |] $ Just [value| L 1     |]
case_Eliminate_AssociativeS      =                
   isoTest [iso| # |+|+| |] [value| R (L 1)    |] $ Just [value| L (R 1) |]
case_Introduce_AssociativeS      =                
   isoTest [iso| ' |+|+| |] [value| L (R 1)    |] $ Just [value| R (L 1) |]
case_Eliminate_SplitS            =                
   isoTest [iso| #  -+<  |] [value| 1          |] $ Nothing
case_Introduce_SplitS            =                
   isoTest [iso| '  -+<  |] [value| 1          |] $ Nothing
case_Eliminate_IdentityP         =                
   isoTest [iso| # <=*=> |] [value| (1, 1)     |] $ Just [value| 1          |]
case_Introduce_IdentityP         =                
   isoTest [iso| ' <=*=> |] [value| 1          |] $ Just [value| (1, 1)     |]
case_Eliminate_CommutativeP      =                
   isoTest [iso| #  x*x  |] [value| (R 1, L 1) |] $ Just [value| (L 1, R 1) |]
case_Introduce_CommutativeP      =                
   isoTest [iso| '  x*x  |] [value| (R 1, L 1) |] $ Just [value| (L 1, R 1) |]
case_Eliminate_AssociativeP      =                
   isoTest [iso| # |*|*| |] [value| (1, (1, 1))|] $ Just [value| ((1, 1), 1)|]
case_Introduce_AssociativeP      =                
   isoTest [iso| ' |*|*| |] [value| ((1, 1), 1)|] $ Just [value| (1, (1, 1))|]
case_Eliminate_DistributiveZero  =                
   isoTest [iso| #  ^0^  |] [value| 1          |] $ Nothing
case_Introduce_DistributiveZero  =                
   isoTest [iso| '  ^0^  |] [value| 1          |] $ Nothing
case_Eliminate_DistributivePlus  =                
   isoTest [iso| #  ^+^  |] [value| ((L 1), 1) |] $ Just [value| L (1, 1)  |]
case_Introduce_DistributivePlus  =                
   isoTest [iso| '  ^+^  |] [value| L (1, 1)   |] $ Just [value| ((L 1), 1)|]

case_TCompose = termTest [term| < # |+|+| ; < ' <=+=> |] [value| L (1, 1) |] $
                Just [value| R (L (L (1, 1))) |]
case_TTimes   = termTest [term| < # |+|+| * < ' <=+=> |] [value| (L 1, 1) |] $
                Just [value| (L (L 1), R 1)   |]
case_TPlus    = termTest [term| < # |+|+| + < ' <=+=> |] [value| R (L 1)  |] $
                Just [value| R (R (L 1))      |]
case_TBase    = termTest [term| < # |+|+|             |] [value| L 1      |] $
                Just [value| L (L 1)          |]
case_TId      = termTest [term| <=>                   |] [value| 1        |] $
                Just [value| 1                |]

tests = $testGroupGenerator










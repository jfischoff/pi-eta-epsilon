{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE QuasiQuotes #-}
module Language.PiEtaEpsilon.Grammar where
import Language.LBNF
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar
import Prelude hiding (Right, Left)


bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;
--                                               
IdentityS.        IsoBase ::= "<=+=>"            ;
IdentityP.        IsoBase ::= "<=*=>"            ;
CommutativeS.     IsoBase ::= "x+x"              ;
CommutativeP.     IsoBase ::= "x*x"              ;
AssociativeS.     IsoBase ::= "|+|+|"            ;
AssociativeP.     IsoBase ::= "|*|*|"            ;
SplitS.           IsoBase ::= "-+<"              ;
SplitP.           IsoBase ::= "-*<"              ;
DistributiveZero. IsoBase ::= "^0^"              ;
DistributivePlus. IsoBase ::= "^+^"              ;

Eliminate.        Iso     ::= "#" IsoBase        ;
Introduce.        Iso     ::= "'" IsoBase        ;

TCompose.         Term     ::= Term  ";" Term1   ;
TSum.             Term1    ::= Term1 "+" Term2   ;
TTimes.           Term2    ::= Term2 "*" Term3   ;
TBase.            Term3    ::= "<" Iso           ;
TId.              Term4    ::= "<=>"             ;

coercions Term 4 ;

TySum.            Typ     ::= Typ  "+" Typ1     ;
TyMinus.          Typ1    ::= Typ1 "-" Typ2     ;
TyProduct.        Typ2    ::= Typ2 "*" Typ3     ;
TyDivides.        Typ3    ::= Typ3 "/" Typ4     ;
TyReciprocate.    Typ4    ::= "/" Typ5          ;
TyNegate.         Typ5    ::= "-" Typ6          ;
TyOne.            Typ6    ::= "1"               ;
TyZero.           Typ7    ::= "0"               ;

coercions Typ 7 ;
                  
VTuple.           Value    ::= Value "," Value1  ;
VLeft.            Value1   ::= "L" Value2        ;
VRight.           Value2   ::= "R" Value3        ;
VNegate.          Value3   ::= "-" Value4        ;
VReciprocate.     Value4   ::= "/" Value5        ;
VUnit.            Value5   ::= "1"               ;

coercions Value 5 ;

comment "/*" "*/" ;
comment "//"      ;

entrypoints Term, Iso, IsoBase, Typ, Value ;
  |]

{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE QuasiQuotes #-}
module Language.PiEtaEpsilon.Grammar where
import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar
import Prelude hiding (Right, Left)


bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;

--                                               
IdentityS.        IsoBase0 ::= "<=+=>"           ;
IdentityP.        IsoBase1 ::= "<=*=>"           ;
CommutativeS.     IsoBase2 ::= "x+x"             ;
CommutativeP.     IsoBase3 ::= "x*x"             ;
AssociativeS.     IsoBase4 ::= "|+|+|"           ;
AssociativeP.     IsoBase5 ::= "|*|*|"           ;
SplitS.           IsoBase6 ::= "-+<"             ;
SplitP.           IsoBase7 ::= "-*<"             ;
DistributiveZero. IsoBase8 ::= "^0^"             ;
DistributivePlus. IsoBase9 ::= "^+^"             ;
--
Eliminate.        Iso0     ::= "#" IsoBase       ;
Introduce.        Iso1     ::= "'" IsoBase       ;
--
TCompose.         Term0    ::= Term1 ";" Term1   ;
TSum.             Term1    ::= Term1 "+" Term2   ;
TTimes.           Term2    ::= Term2 "*" Term3   ;
TBase.            Term3    ::= "<" Iso           ;
TId.              Term4    ::= "<=>"             ;

TySum.            Typ0     ::= Typ0 "+" Typ1     ;
TyProduct.        Typ1     ::= Typ1 "*" Typ2     ;
TyReciprocate.    Typ2     ::= "/" Typ3          ;
TyNegate.         Typ3     ::= "-" Typ4          ;
TyOne.            Typ4     ::= "1"               ;
TyZero.           Typ5     ::= "0"               ;
                  
VTuple.           Value1   ::= Value1 "," Value2 ;
VLeft.            Value2   ::= "L" Value3        ;
VRight.           Value3   ::= "R" Value4        ;
VNegate.          Value4   ::= "-" Value5        ;
VReciprocate.     Value5   ::= "/" Value6        ;
VUnit.            Value6   ::= "1"               ;


-- Coecersions boilerplate
_. Term     ::= Term0           ;
_. Term0    ::= Term1           ;
_. Term1    ::= Term2           ;
_. Term2    ::= Term3           ;
_. Term3    ::= Term4           ;
_. Term4    ::= "(" Term ")"    ;
            
_. Iso      ::= Iso0            ;
_. Iso0     ::= Iso1            ;
_. Iso1     ::= "(" Iso ")"     ;
            
_. IsoBase  ::= IsoBase0        ;  
_. IsoBase0 ::= IsoBase1        ;
_. IsoBase1 ::= IsoBase2        ;
_. IsoBase2 ::= IsoBase3        ;
_. IsoBase3 ::= IsoBase4        ;
_. IsoBase4 ::= IsoBase5        ;
_. IsoBase5 ::= IsoBase6        ;
_. IsoBase6 ::= IsoBase7        ;
_. IsoBase7 ::= IsoBase8        ;
_. IsoBase8 ::= IsoBase9        ;
_. IsoBase9 ::= "(" IsoBase ")" ;
            
_. Typ      ::= Typ0            ;
_. Typ0     ::= Typ1            ;
_. Typ1     ::= Typ2            ;
_. Typ2     ::= Typ3            ;
_. Typ3     ::= Typ4            ;
_. Typ4     ::= Typ5            ;
_. Typ5     ::= "(" Typ ")"     ;
                                
_. Value    ::= Value0          ;
_. Value0   ::= Value1          ;
_. Value1   ::= Value2          ;
_. Value2   ::= Value3          ;
_. Value3   ::= Value4          ;
_. Value4   ::= Value5          ;
_. Value5   ::= Value6          ;
_. Value6   ::= "(" Value ")"   ;

comment "/*" "*/" ;
comment "//" ;

entrypoints Term, Iso, IsoBase, Typ, Value ;
  |]
  

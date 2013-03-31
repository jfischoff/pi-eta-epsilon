{-# LANGUAGE TemplateHaskell    #-} 
{-# LANGUAGE QuasiQuotes        #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Grammar where
import Language.PiEtaEpsilon.Grammar
import Language.PiEtaEpsilon.Parser
import Test.Framework.TH
import Test.Framework.Providers.Feat
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.Feat
import Data.Typeable
import Language.LBNF.Runtime
import Control.Applicative
import Data.Monoid
import Test.Framework.Options
import Language.PiEtaEpsilon.Sugar
import Test.HUnit
import Prelude hiding (Right, Left)

-- This is mostly checking the quasiquoter
case_parseTermIsobase_0 = IdentityS         @?= [isoBase| <=+=> |]    
case_parseTermIsobase_1 = IdentityP         @?= [isoBase| <=*=> |]
case_parseTermIsobase_2 = CommutativeS      @?= [isoBase| x+x   |]
case_parseTermIsobase_3 = CommutativeP      @?= [isoBase| x*x   |]
case_parseTermIsobase_4 = AssociativeS      @?= [isoBase| |+|+| |]
case_parseTermIsobase_5 = AssociativeP      @?= [isoBase| |*|*| |]
case_parseTermIsobase_6 = SplitS            @?= [isoBase|  -+<  |]
case_parseTermIsobase_7 = SplitP            @?= [isoBase|  -*<  |]
case_parseTermIsobase_8 = DistributiveZero  @?= [isoBase|  ^0^  |]
case_parseTermIsobase_9 = DistributivePlus  @?= [isoBase|  ^+^  |]

case_parseTermIso_10 = Eliminate AssociativeS @?= [iso| #  |+|+| |] 
case_parseTermIso_11 = Introduce AssociativeP @?= [iso| '  |*|*| |] 

case_parseTermTerm_12 = (TBase . Eliminate $ AssociativeS) @.
                        (TBase . Introduce $ AssociativeS)     @?=
                        [term| < # |+|+| ; < ' |+|+| |]
case_parseTermTerm_13 = (TBase . Eliminate $ IdentityS) @*
                        (TBase . Introduce $ AssociativeS)     @?=
                        [term| < # <=+=> * < ' |+|+| |]
case_parseTermTerm_14 = (TBase . Eliminate $ IdentityS) @+
                        (TBase . Introduce $ AssociativeS)     @?= 
                        [term| < # <=+=> + < ' |+|+| |]
case_parseTermTerm_15 = (TBase . Eliminate $ AssociativeP)     @?= 
                        [term| < # |*|*| |] 
case_parseTermTerm_16 = TId @?= [term| <=> |] 

case_parseValue_0 = VTuple       VUnit VUnit @?= [value| (1,1) |]
case_parseValue_1 = VLeft        VUnit       @?= [value| L 1   |]
case_parseValue_2 = VRight       VUnit       @?= [value| R 1   |]
case_parseValue_3 = VNegate      VUnit       @?= [value| - 1   |]
case_parseValue_4 = VReciprocate VUnit       @?= [value| / 1   |]
case_parseValue_5 = VUnit                    @?= [value| 1     |]

tests = $testGroupGenerator
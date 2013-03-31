-- boilerplate {{{1
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}
module Language.PiEtaEpsilon.Types where
import Control.Applicative 
import Control.Unification
import Data.Foldable
import Data.Functor.Fixedpoint
import Data.Traversable
import Prelude hiding (negate)
import GHC.Generics
import Data.Data
import Data.Typeable
import Data.Default
import Language.PiEtaEpsilon.Grammar
import Language.PiEtaEpsilon.Sugar
import Data.Function
import Control.Unification.IntVar
import Text.PrettyPrint.Free

deriving instance Generic  IsoBase
deriving instance Typeable IsoBase
deriving instance Data     IsoBase

deriving instance Generic  Iso
deriving instance Typeable Iso
deriving instance Data     Iso

deriving instance Generic  Term
deriving instance Typeable Term
deriving instance Data     Term

deriving instance Generic  Typ
deriving instance Typeable Typ
deriving instance Data     Typ

deriving instance Generic  Value
deriving instance Typeable Value
deriving instance Data     Value

instance Default IsoBase
instance Default Iso
instance Default Term

class PrettyPrec a where 
    prettyPrec :: Int -> a -> Doc e
    
cparens :: Int -> Int -> Doc e -> Doc e
cparens thisP otherP doc 
    | thisP <= otherP = parens doc
    | otherwise       = doc
    
instance Pretty (Fix ValueF) where
    pretty = prettyPrec (-1)

instance PrettyPrec (Fix ValueF) where
    prettyPrec p v = case unFix v of
        VFUnit            -> text "1"
        VFLeft        x   -> cparens 0 p $ text "L" <+> prettyPrec 0 x
        VFRight       x   -> cparens 0 p $ text "R" <+> prettyPrec 0 x
        VFTuple       x y -> parens $ prettyPrec (-1) x <> comma <+> prettyPrec (-1) y
        VFNegate      x   -> cparens 1 p $ text "-" <> prettyPrec 1 x
        VFReciprocate x   -> cparens 1 p $ text "/" <> prettyPrec 1 x

-- values and unification variables {{{2
data ValueF t
   = VFUnit
   | VFLeft        t
   | VFRight       t
   | VFTuple       t t
   | VFNegate      t
   | VFReciprocate t
   deriving (Eq, Ord, Show, Read, Data, Typeable, 
            Generic, Functor, Foldable, Traversable)

type FixedValue = Fix ValueF

-- convenience names for Values {{{1
class Particle a where
	unit :: a
	left, right, negate, reciprocate :: a -> a
	tuple :: a -> a -> a

instance Particle FixedValue where
	unit        = Fix VFUnit
	left        = Fix . VFLeft
	right       = Fix . VFRight
	negate      = Fix . VFNegate
	reciprocate = Fix . VFReciprocate
	tuple t1 t2 = Fix (VFTuple t1 t2)

instance Particle (UTerm ValueF v) where
	unit        = UTerm VFUnit
	left        = UTerm . VFLeft
	right       = UTerm . VFRight
	negate      = UTerm . VFNegate
	reciprocate = UTerm . VFReciprocate
	tuple t1 t2 = UTerm (VFTuple t1 t2)

-- doesn't need to be part of a type class yet, I guess
var = UVar

toValueF :: Value -> FixedValue
toValueF = go where
    go e = case e of
       VTuple       x y -> Fix $ VFTuple (go x) (go y)
       VLeft        x   -> Fix $ VFLeft         (go x)
       VRight       x   -> Fix $ VFRight        (go x)
       VNegate      x   -> Fix $ VFNegate       (go x)
       VReciprocate x   -> Fix $ VFReciprocate  (go x)
       VUnit            -> Fix VFUnit

toP :: (Particle a) => FixedValue -> a
toP v = case v of
   Fix (VFTuple        x y) -> tuple       (toP x) (toP y)
   Fix (VFLeft         x  ) -> left        (toP x)
   Fix (VFRight        x  ) -> right       (toP x)
   Fix (VFNegate       x  ) -> negate      (toP x)
   Fix (VFReciprocate  x  ) -> reciprocate (toP x)
   Fix VFUnit               -> unit

type UValue = UTerm ValueF IntVar

instance Default UValue where
    def = unit

adjointIso :: Iso -> Iso
adjointIso e = case e of
    Eliminate b -> Introduce b
    Introduce b -> Eliminate b

adjoint :: Term -> Term
adjoint e = case e of
   TBase    iso   -> TBase (adjointIso iso)
   TId            -> TId
   TCompose t1 t2 -> adjoint t2 @. adjoint t1
   TSum     t1 t2 -> adjoint t1 @+ adjoint t2
   TTimes    t1 t2 -> adjoint t1 @* adjoint t2

-- unification for UValues {{{1
instance Unifiable ValueF where
   zipMatch  VFUnit               VFUnit              = Just VFUnit
   zipMatch (VFLeft        a   ) (VFLeft        b   ) = Just (VFLeft   $ Right  (a, b)         )
   zipMatch (VFRight       a   ) (VFRight       b   ) = Just (VFRight  $ Right  (a, b)         )
   zipMatch (VFTuple       a a') (VFTuple       b b') = Just (VFTuple   (Right   (a, b)) (Right (a', b')))
   zipMatch (VFNegate      a   ) (VFNegate      b   ) = Just (VFNegate $ Right       (a, b)         )
   zipMatch (VFReciprocate a   ) (VFReciprocate b   ) = Just (VFReciprocate $ Right (a, b)         )
   zipMatch _ _ = Nothing

closeValue :: UValue -> Maybe FixedValue
closeValue e = case e of
   UTerm  VFUnit           -> return unit
   UTerm (VFLeft   v     ) -> left        <$> closeValue v
   UTerm (VFRight  v     ) -> right       <$> closeValue v
   UTerm (VFTuple  v1 v2 ) -> tuple       <$> closeValue v1 <*> closeValue v2
   UTerm (VFNegate v     ) -> negate      <$> closeValue v
   UTerm (VFReciprocate v) -> reciprocate <$> closeValue v
   _                     -> empty

closedAndEqual :: UValue -> UValue -> Bool
closedAndEqual = (==) `on` closeValue



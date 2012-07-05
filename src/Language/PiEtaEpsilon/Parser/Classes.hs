{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses,
  TypeSynonymInstances, FunctionalDependencies, FlexibleInstances #-}

module Language.PiEtaEpsilon.Parser.Classes where

class To a b | a -> b where
    to :: a -> b 

class From a b | a -> b where
    from :: a -> b

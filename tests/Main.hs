{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE QuasiQuotes               #-}
module Main where
import Evaluator as E
import Grammar   as G
import Parser    as P
import Control.Applicative
import Data.Monoid
import Test.Framework.Options
import Test.Framework

defOptions = mempty

main = defaultMainWithOpts [
        E.tests
      , G.tests
      , P.tests
    ] (defOptions { 
      ropt_test_options = 
         (\x -> x { topt_maximum_generated_tests = Just 1000 })
            <$> Just mempty })
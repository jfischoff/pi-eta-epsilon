{-# LANGUAGE TemplateHaskell    #-} 
{-# LANGUAGE QuasiQuotes        #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Parser where
import Language.PiEtaEpsilon.Grammar
import Language.PiEtaEpsilon.Parser
import Language.PiEtaEpsilon.Types
import Test.Framework.TH
import Test.Framework.Providers.Feat
import Test.Framework
import Test.Feat
import Data.Typeable
import Language.LBNF.Runtime


deriveEnumerable ''Term
deriveEnumerable ''Iso
deriveEnumerable ''IsoBase
deriveEnumerable ''Value
deriveEnumerable ''Typ

printParseRTrip :: (Eq a, Show a, Print a) 
                => (String -> ParseResult a) -> a -> Bool
printParseRTrip f x = either (const False) (x==) . f . printTree $ x

printParsePrint :: (Eq a, Show a, Print a) 
                => (String -> ParseResult a) -> a -> Bool
printParsePrint f x = 
    either (const False) ((printTree x ==) . printTree) . f . printTree $ x

-- properties
feat_Term_Print_to_Parse_RoundTrip :: Term -> Bool
feat_Term_Print_to_Parse_RoundTrip = printParseRTrip parse

feat_Iso_Print_to_Parse_RoundTrip :: Iso -> Bool
feat_Iso_Print_to_Parse_RoundTrip = printParseRTrip parse

feat_IsoBase_Print_to_Parse_RoundTrip :: IsoBase -> Bool
feat_IsoBase_Print_to_Parse_RoundTrip = printParseRTrip parse

feat_Value_Print_to_Parse_RoundTrip :: Value -> Bool
feat_Value_Print_to_Parse_RoundTrip = printParseRTrip parse

feat_Type_Print_to_Parse_RoundTrip :: Typ -> Bool
feat_Type_Print_to_Parse_RoundTrip = printParseRTrip parse

tests = $testGroupGenerator
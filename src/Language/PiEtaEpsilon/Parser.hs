module Language.PiEtaEpsilon.Parser (
    term,
    value,
    iso,
    isoBase,
    typ,
    Parsable,
    parse,
    ParseResult 
    ) where
import Language.PiEtaEpsilon.Grammar
import Language.LBNF.Runtime
import Prelude hiding (Right, Left)
import Prelude as P

type ParseResult = Either String

toResult (Ok x)  = P.Right x
toResult (Bad x) = P.Left x

class Parsable a where
    parse :: String -> ParseResult a

instance Parsable Term where
    parse = toResult . pTerm . myLexer 

instance Parsable Value where
    parse = toResult . pValue . myLexer 

instance Parsable IsoBase where
    parse = toResult . pIsoBase . myLexer 

instance Parsable Iso where
    parse = toResult . pIso . myLexer 

instance Parsable Typ where
    parse = toResult . pTyp . myLexer

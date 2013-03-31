module System.Console.PiEtaEpsilon.StatementParser (
    Statement (..),
    pStatement
    ) where
import Language.PiEtaEpsilon
import Control.Monad.Identity
import Data.List.Split

toUV = toP . toValueF

data Statement = Stmt_eval Term UValue
               | Stmt_let String Term
               | Stmt_empty
   
pStatement :: String -> Either String Statement    
pStatement input = do
    [termString, valueString] <- case splitOn "$" input of
        [t, v] -> return [t, v]
        _ -> Left "Statement parser error! Did you forget a '$' between the term and the value?"
    term  <- parse termString
    value <- parse valueString
    return $ Stmt_eval term (toUV value)

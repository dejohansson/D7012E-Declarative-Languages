-- David Johansson
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = Statement.showStmts 0 stmts

exec :: T -> [Integer] -> [Integer]  
exec (Program p) i = Statement.exec p (Dictionary.empty) i

exProg :: Maybe (T, String) -> [Statement.T]
exProg Nothing = error "Couldn't extract program!"
exProg (Just (Program prog, str)) = prog
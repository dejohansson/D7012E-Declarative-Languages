module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program () -- to be defined
instance Parse T where
  parse = iter Statement.parse
  toString = error "Program.toString not implemented"
             
exec p i = Statement.exec (parse p) Dictionary.empty i

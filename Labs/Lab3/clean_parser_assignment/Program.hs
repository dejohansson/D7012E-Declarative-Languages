module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = error "Program.toString not implemented"

exec :: String -> [Integer] -> [Integer]  
exec p i = error "" --case (parse p) of
             --Just (prog, str) -> Statement.exec prog (Dictionary.empty) i
             --Nothing -> error ""

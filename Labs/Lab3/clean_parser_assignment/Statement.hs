module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |      --Original
    Skip |
    Begin Statements | --Multiple Statements ????
    If Expr.T Statement Statement | --Original
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

type Statements = [Statement]

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss --Original
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> \_ -> Skip

begin = accept "begin" -# statements #- require "end" >-> \sl -> Begin sl

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> \v -> Read v

write = accept "write" -# Expr.parse #- require ";" >-> \e -> Write e

statements = iter parse

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment v e: stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Skip: stmts) dict input = exec stmts dict input
exec (Begin stmtList: stmts) dict input = exec (stmtList++stmts) dict input
exec (If cond thenStmts elseStmts: stmts) dict input =     --Original
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmt: stmts) dict input = 
    if (Expr.value cond dict)>0
    then exec [stmt, (While cond stmt)] dict input
    else exec stmts dict input
exec (Read v: stmts) dict (input: inputs) = exec stmts (Dictionary.insert (v, input) dict) inputs
exec (Write e: stmts) dict input = (Expr.value e dict) : (exec stmts dict input)

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStmt ! while ! readStmt ! write   --error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"

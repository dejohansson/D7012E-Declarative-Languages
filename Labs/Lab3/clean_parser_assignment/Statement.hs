-- David Johansson
module Statement(T, parse, toString, fromString, exec, showStmts) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |      --Original
    Skip |
    Begin Statements |
    If Expr.T Statement Statement | --Original
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Repeat Statement Expr.T
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

repeatStmt = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s, e)= Repeat s e

statements = iter parse

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment v e: stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Skip: stmts) dict input = exec stmts dict input
exec (Begin stmtList: stmts) dict input = exec (stmtList++stmts) dict input
exec (If cond thenStmt elseStmt: stmts) dict input =     --Original
    if (Expr.value cond dict)>0 
    then exec (thenStmt: stmts) dict input
    else exec (elseStmt: stmts) dict input
exec (While cond stmt: stmts) dict input = 
    if (Expr.value cond dict)>0
    then exec ([stmt, (While cond stmt)]++stmts) dict input
    else exec stmts dict input
exec (Read v: stmts) dict (input: inputs) = exec stmts (Dictionary.insert (v, input) dict) inputs
exec (Write e: stmts) dict input = (Expr.value e dict) : (exec stmts dict input)
exec (Repeat stmt cond: stmts) dict input = exec ([stmt, (If cond Skip (Repeat stmt cond))]++stmts) dict input

tabN :: Int -> String
tabN 0 = ""
tabN n = "  " ++ (tabN (n-1))

showStmts :: Int -> Statements -> String
showStmts _ [] = ""
showStmts n (stmt: stmts) = (showStmt n stmt) ++ (showStmts n stmts)

showStmt :: Int -> Statement -> String
showStmt n (Assignment v e) = (tabN n) ++ v ++ " := " ++ (Expr.toString e) ++ ";\n"
showStmt n (Skip) = (tabN n) ++ "skip;\n"
showStmt n (Begin stmtList) = (tabN n) ++ "begin\n" ++ (showStmts (n+1) stmtList) 
                              ++ (tabN n) ++ "end\n"
showStmt n (If cond thenStmt elseStmt) = (tabN n) ++ "if " ++ (Expr.toString cond) 
                                          ++ " then\n" ++ (showStmt (n+1) thenStmt) 
                                          ++ (tabN n) ++ "else\n" ++ (showStmt (n+1) elseStmt)
showStmt n (While cond stmt) = (tabN n) ++ "while " ++ (Expr.toString cond) ++ " do\n" 
                                ++ (showStmt (n+1) stmt)
showStmt n (Read v) = (tabN n) ++ "read " ++ v ++ ";\n"
showStmt n (Write e) = (tabN n) ++ "write " ++ (Expr.toString e) ++ ";\n"
showStmt n (Repeat stmt cond) = (tabN n) ++ "repeat\n" ++ (showStmt (n+1) stmt) ++ 
                                (tabN n) ++ "until " ++ (Expr.toString cond) ++ ";\n"

instance Parse Statement where
  parse =   assignment ! skip ! begin ! ifStmt ! 
            while ! readStmt ! write ! repeatStmt
  toString = showStmt 0

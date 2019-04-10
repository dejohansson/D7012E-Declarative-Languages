--David Johansson

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App str ex) = str ++ "(" ++ unparse ex ++ ")" --Added

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App f ex) env    --Added
  | f == "sin" = (sin (eval ex env))
  | f == "cos" = (cos (eval ex env))
  | f == "log" = (log (eval ex env))
  | f == "exp" = (exp (eval ex env))

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App str ex)     --Added
  | str == "sin" = Op "*" (App "cos" ex) (diff v ex)
  | str == "cos" = Op "*" (Op "-" (Const 0) (App "sin" ex)) (diff v ex)
  | str == "log"  = Op "/" (diff v ex) ex
  | str == "exp"  = Op "*" (App "exp" ex) (diff v ex)
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App str ex) = App str (simplify ex) --Added

-- Part 3
type Fun = (Float -> Float)
mkfun :: (EXPR, EXPR) -> Fun
mkfun (body, Var var) = \x -> eval body [(var,x)]

-- Part 4
findzeroHelp :: Fun -> Fun -> Float -> Float
findzeroHelp f f' x = if abs (x - x') > 0.0001 then (findzeroHelp f f' x') else x'
                        where x' = x - (f x) / (f' x)

findzero :: String -> String -> Float -> Float
findzero var body x = if f' x == 0 then x else findzeroHelp f f' x
                        where f  = mkfun ((parse body), (Var var))
                              f' = mkfun ((diff (Var var) (parse body)), (Var var))

-- Test Examples:
-- unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))

-- myFun = mkfun (parse "x*x+2", Var "x")
-- myFun 3

-- findzero "x" "x*x*x+x-1" 1.0       = 0.68232775
-- findzero "y" "cos(y)*sin(y)" 2.0   = 1.5707964
-- findzero "z" "z*z-1" 5.0           = 1
-- findzero "z" "z*z-1" (-5.0)        = -1
{- Test for Expr-}
module TestExpr where

import qualified Dictionary
import Expr

dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty 

testValue string = value (fromString string) dict

n1 = testValue "1"
n2 = testValue "x"
n3 = testValue "x+y"
n4 = testValue "x-y-y"
n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}
n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}


-- Extra tests
n100 = testValue "y"
n101 = testValue "f"
n102 = testValue "3*y"
n103 = testValue "10/y"

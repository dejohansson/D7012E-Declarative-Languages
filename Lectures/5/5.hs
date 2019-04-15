-- 3

data Box a b = Slot a a b


-- 4

data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter

weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold


-- 5

data Person1 = Adult1 String String Int

nemo = Adult1 "Clownfish Nemo" "The Reef" 7
peter = Adult1 "Peter Pan"  "Neverland" 13

sumAge1 :: [Person1] -> Int
sumAge1 [] = 0
sumAge1 ( (Adult1  _  _ a) : xs ) = a + sumAge1 xs


type Name = String
type Address = String
type Age = Int

data Person2 = Adult2 Name Address Age

batman = Adult2 "Bruce Wayne"  "Gotham City" 35

sumAge2 :: [Person2] -> Age
sumAge2 [] = 0
sumAge2 ( (Adult2 _ _ a) : xs ) = a + sumAge2 xs

-- *Main> :type sumAge2 [batman]
-- 35
-- *Main> :type sumAge2 [batman]
-- sumAge2 [batman] :: Age
-- *Main> :type sumAge1 [nemo,peter]
-- sumAge1 [nemo,peter] :: Int
-- *Main> :type sumAge2 [batman]
-- sumAge2 [batman] :: Age
-- *Main> sumAge2 [batman]
-- 35
-- *Main> :type it
-- it :: Age
-- *Main> sumAge1 [nemo,peter]
-- 20
-- *Main> :type it
-- it :: Int
-- *Main>


-- 6

data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show)

area :: Shape -> Float
area (Circle r) = 3.14 * r * r
area (Rectangle w h) = w * h


-- 7

type Price = Float
data StoreItem t = Item t Price

s1 = Item "book"  25.0 :: StoreItem String
s2 = Item (Circle 1.0) 1000.0 :: StoreItem Shape

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)


-- 9

data IntList = IntNil | IntCons Int  IntList deriving (Eq, Ord, Show)

il1 = IntCons 4 
  (IntCons 2 
    (IntCons 1 
      (IntCons 3 
        IntNil ))):: IntList 

length2:: IntList -> Int
length2 IntNil = 0
length2 (IntCons _ xs) = 1 + length2 xs

prependZero :: IntList -> IntList 
prependZero list = IntCons 0 list


-- 10

data List u = Nil | Cons u (List u) deriving (Eq, Ord, Show)

pll1 = Cons 'a'
  (Cons 'x'
    (Cons 'c'
      (Cons 'r'
        Nil ))) :: List Char

-- pll2 == pll1, just written on a single line
pll2 = Cons 'a' (Cons 'x' (Cons 'c' (Cons 'r' Nil ))) :: List Char

length3:: List p -> Int
length3 Nil = 0
length3 (Cons _ xs) = 1 + length3 xs

prependDash :: List Char -> List Char
prependDash list = Cons '-' list

-- example
append2 :: List u -> List u -> List u
append2 Nil list = list
append2 (Cons x xs) list = Cons x (append2 xs list)


-- 11

data IntTree = IntEmpty |
               IntNode IntTree Int IntTree
               deriving (Eq, Ord, Show)

mbt1 =
   IntNode 
     (IntNode IntEmpty 4 (IntNode IntEmpty 2 IntEmpty)) 
     1 
     (IntNode IntEmpty 3 IntEmpty) :: IntTree

size :: IntTree -> Int
size IntEmpty = 0
size (IntNode left _ right) = 1 + size left + size right


-- 12

data Tree q = Empty | Node (Tree q) q (Tree q) deriving (Eq, Ord, Show)

pbt1 =
  Node 
   (Node Empty 4.0 (Node Empty 2.0 Empty)) 
   1.0 
   (Node Empty 3.0 Empty) :: Tree Float

size2 :: Tree w -> Int
size2 Empty = 0
size2 (Node left _ right) = 1 + size2 left + size2 right


-- 13

depthT :: Tree t  ->  Int
depthT  Empty =  0
depthT  (Node  t1  _  t2)  = 
  1 + max (depthT  t1) (depthT  t2)

collapse :: Tree d  ->  [ d ]
collapse  Empty =  []
collapse  (Node t1  x  t2)  = 
  collapse t1  ++  [x]  ++  collapse t2


-- 14

data MyTree a = EmptyTree | TreeNode a (MyForest a)

data MyForest b = EmptyForest | ForestNode (MyTree b) (MyForest b) 

data Person =
  Adult Name Address Age Biog |
  Child Name

data Biog = Parent String [Person] | NonParent String

-- extra clearity with named types
type Information = String
type ListOfChildren = [Person]

data Biog2 =
  Parent2 Information ListOfChildren |
  NonParent2 Information


-- 15

-- data EXPR = Const Int
--      | Var String
--      | Op String EXPR EXPR
--      | App String EXPR deriving (Eq, Ord, Show)

e1 = Op "/" (App "sin" (Var "x")) (Op "+" (Var "x") (Const 1))

count :: EXPR -> Int
count (Const _) = 1
count (Var _) = 1
count (Op _ e1 e2) = 1 + count e1 + count e2
count (App _ e) = 1 + count e


-- 16

-- data Maybe a = Nothing | Just a
--   deriving (Eq,Ord,Show)

errDiv1 :: Int -> Int -> Int
errDiv1 n m 
  | (m /= 0) = n `div` m
  | otherwise = error "Div by 0"

errDiv2 :: Int -> Int -> Maybe Int
errDiv2 n m 
  | (m /= 0) = Just (n `div` m)
  | otherwise = Nothing 


-- 17

mapMaybe :: (a -> b)  ->  Maybe a  ->  Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe g (Just x) = Just (g x)

maybe :: b  ->  (a -> b)  ->  Maybe  a -> b
maybe  n  _  Nothing  =  n
maybe  _  f  (Just x)  =  f x


-- 18

data Vector = Vec Float Float

class Movable a where
  	move      		:: Vector -> a -> a
  	reflectX  		:: a -> a
  	reflectY  		:: a -> a
  	rotate180	:: a -> a
  	rotate180 = reflectX . reflectY

data Point = Point Float Float 
             deriving Show

instance Movable Point where
	move (Vec v1 v2) (Point c1 c2) = 
		Point (c1+v1) (c2+v2)
  	reflectX (Point c1 c2)  = Point c1 (-c2)
  	reflectY (Point c1 c2)  = Point (-c1) c2
  	rotate180 (Point c1 c2) = Point (-c1) (-c2)

data Figure = Line Point Point |
              Circle2 Point Float 
              deriving Show

instance Movable Figure where
  	move v (Line p1 p2) = 
		Line (move v p1) (move v p2)
  	move v (Circle2 p r) = Circle2 (move v p) r
	reflectX (Line p1 p2) = 
		Line (reflectX p1) (reflectX p2)
	reflectX (Circle2 p r) = Circle2 (reflectX p) r
	reflectY (Line p1 p2) = 
		Line (reflectY p1) (reflectY p2)
	reflectY (Circle2 p r) = Circle2 (reflectY p) r

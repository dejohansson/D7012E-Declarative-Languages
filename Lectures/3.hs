

-- 10

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs


doubleAll1 :: [Int] -> [Int]
doubleAll1 xs = map double xs
  where
   double x = 2*x

doubleAll2 :: [Int] -> [Int]
doubleAll2 = map double
     where
       double x = 2*x


-- 11

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x : xs)
  | p x  =  x : filter1 p xs
  | otherwise = filter1 p xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = [ x | x <- xs, p x ]


evenList1 :: [Int] -> [Int]
evenList1 xs = filter isEven xs
  where
    isEven :: Int -> Bool
    isEven n = n `mod` 2 == 0

evenList2 :: [Int] -> [Int]
evenList2 = filter isEven
  where
    isEven :: Int -> Bool
    isEven n = n `mod` 2 == 0


-- 12

-- foldr is predefined

map3 f xs = foldr f' [] xs
    where
        f' x xs = (f x):xs


filter3 f xs = foldr f' [] xs
    where
        f' x xs = if f x then x:xs else xs


-- 13

fac :: Int-> Int
fac 0 = 1
fac n = n * fac (n - 1)

foldrFac :: Int -> Int
foldrFac n = foldr (*) 1 [1..n]

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y : ys) = if x <= y then x : y : ys else y : ins x ys 

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x : xs) = ins x (iSort xs)

flodriSort1 :: [Int] -> [Int]
flodriSort1 xs = foldr ins [] xs

flodriSort2 :: [Int] -> [Int]
flodriSort2 = foldr ins [] 

concat' :: [[t]] -> [t]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

foldrConcat1 :: [[t]] -> [t]
foldrConcat1 xs = foldr (++) [] xs

foldrConcat2 :: [[t]] -> [t]
foldrConcat2 = foldr (++) []

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

foldrSum1 :: [Int] -> Int
foldrSum1 xs = foldr (+) 0 xs

foldrSum2 :: [Int] -> Int
foldrSum2 = foldr (+) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [ x]

rev :: [w] -> [w]
rev [] = []
rev (x : xs) = rev xs  ++ [ x ]

foldrRev1 :: [w] -> [w]
foldrRev1 xs = foldr snoc [] xs

foldrRev2 :: [w] -> [w]
foldrRev2 = foldr snoc []


-- 15

posList1 :: [Int] -> [Int]
posList1 xs = filter (\n -> n>=0) xs

posList2 :: [Int] -> [Int]
posList2 = filter (\n -> n>=0)

squareAll1 :: [ Int ] -> [ Int ]
squareAll1 xs = map ( \v -> v * v ) xs

squareAll2 :: [ Int ] -> [ Int ]
squareAll2 = map ( \v -> v * v ) 


foldrRev3 :: [a] -> [a]
foldrRev3 = foldr (\x ra -> ra ++ [x]) []


-- 16

map4 f = foldr (\x->(\xs->(f x):xs)) []

filter4 f = foldr (\x xs -> if (f x) then (x:xs) else xs) []


-- 17

powerset1 :: [t] -> [[t]]
powerset1 [] = [[]]
powerset1 (x:xs) = powerset1 xs ++ map (x:) (powerset1 xs)

powerset2 :: [t] -> [[t]]
powerset2 =
  foldr (\x acc -> acc ++ (map (x:) acc)) [[]]


-- 18

-- curry and uncurry from the course book


-- Bonus candy



nat :: [Int] -- from last lecture
nat = 0 : [ x+1 | x <- nat ]

int :: [Int] -- all integers, listed from 0
int = 0 : (concat.tail) [ [x,-x] | x <- nat ] 
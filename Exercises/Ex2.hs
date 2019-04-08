-- Chapter 5
-- Ex 5.2
maxThree :: (Int, Int, Int) -> Int
maxThree (a, b, c)
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | c >= a && c >= b = c

middleThree :: (Int, Int, Int) -> Int
middleThree (a, b, c)
    | (b >= a && a >= c) || (c >= a && a >= b) = a
    | (a >= b && b >= c) || (c >= b && b >= a) = b
    | (c >= c && c >= b) || (b >= c && c >= c) = c

minThree :: (Int, Int, Int) -> Int
minThree (a, b, c)
    | a <= b && a <= c = a
    | b <= a && b <= c = b
    | c <= a && c <= b = c

orderTripple :: (Int, Int, Int) -> (Int, Int, Int)
orderTripple t = (minThree t, middleThree t, maxThree t)

-- Ex 5.10
divisor :: Int -> [Int]
divisor n = [x | x <- [1..n], (n `mod` x) == 0]

-- Ex 5.11
matches :: Int -> [Int] -> [Int]
matches n l = [x | x <- l, x == n]

elem :: Int -> [Int] -> Bool
elem n l = if matches n l == [] then False else True

-- Ex 5.18
shift ((x,y),z) = (x,(y,z)) --Use <:type shift> to see type

-- Ex 5.22
osl :: [String] -> String --Use putStr to format the output
osl l 
    | l == [] = ""
    | otherwise = (head l ++ "\n") ++ osl (tail l)

-- Ex 5.23
duplicate :: String -> Int -> String
duplicate s n
    | n <= 0 = ""
    | otherwise = s ++ duplicate s (n-1)

-- Ex 5.24
pushRight :: Int -> String -> String 
pushRight n s 
    | n <= 0 = ""
    | n - length s > 0 = " " ++ pushRight (n-1) s
    | otherwise = head s : pushRight (n-1) (tail s)

-- Chapter 6
-- Ex 6.29
{- 
Haskell Stores:
Dry Sherry, llt ........... 5.40
Fish Fingers .............. 1.21
Orange Jelly .............. 0.56
Hula Hoops (Giant) ........ 1.33
Unknown Item .............. 0.00
Dry Sherry, llt ........... 5.40
Discount .................. 1.00
Total .................... 12.90  -}

--Bar codes: [1234, 4791, 3814, 1112, 1113, 1234]

--type billType = [(Int, String, Int)] -- Nr, Name, Price
mergesort'merge :: (Ord a) => [a] -> [a] -> [a]
mergesort'merge [] xs = xs
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)
    | (x < y) = x:mergesort'merge xs (y:ys)
    | otherwise = y:mergesort'merge (x:xs) ys
 
mergesort'splitinhalf :: [a] -> ([a], [a])
mergesort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort'splitinhalf xs

codes = [1234, 4791, 3814, 1112, 1113, 1234] :: [Int]
--discount :: [Int] -> Int
--discount l = []


-- Chapter 7
-- Ex 7.2
listMatch :: [Int] -> Int
listMatch [] = 0
listMatch (x : []) = x
listMatch (x : y : _) = x+y
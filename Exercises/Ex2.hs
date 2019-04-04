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
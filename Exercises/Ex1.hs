--Ex 4.9
f :: Int -> Int
f a = a*(5-a)

listMax :: [Int] -> Int
listMax [] = 0
listMax (x : xs) = if x > y then x else y
    where
        y = listMax xs

maxF :: Int -> Int
maxF n = listMax [f x | x <- [1..n]]

--Ex 4.14
powerOfTwo :: Int -> Float
powerOfTwo n
    | n == 0 = 1
    | otherwise =   if n > 0 then 
                        2 * powerOfTwo (n-1) 
                    else 
                        (powerOfTwo (n+1))/2
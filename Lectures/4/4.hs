import Data.Char

-- 3

f4_1 :: Int ->  [a] -> Int
f4_1  =  
   \x  ->  \ ls  -> x + length ls


f4_2 :: Eq u => [w] ->  [u] -> Int
f4_2  =  
   \xs ys  -> length xs +
     (if head ys == head (tail ys) then 1 else 2)


-- 5

class Weighable t where
  weight :: t -> Float

instance Weighable Char where
  weight = fromIntegral . ord  -- the wieght is the ASCII number

instance Weighable Bool where
  weight False = 0
  weight True = 1

instance Weighable Int where
  weight = sqrt . fromIntegral

instance Weighable u => Weighable [u] where
  weight = foldr (+) 0 . map weight

price  ::  Weighable t  =>  t  -> Int
price thing
  | weight thing < 100.0 = 60
  | weight thing < 200.0 = 90
  | otherwise = 110

-- instance Weighable Int


-- 13

hello x y
   | x == '-'  =  y
   | x == y =  x


-- 16

expr = length ([]++[True]) + length ([]++[2,3,4])


-- 17

mult x y = x * y
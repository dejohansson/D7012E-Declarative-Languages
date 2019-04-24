mkList :: Int -> Int -> [Int]
mkList f s = [f, s] ++ mkList (s+(s-f)) (s+2*(s-f))

data Tree a = 
    Node (a, Int) (Tree a) (Tree a) (Tree a) |
    Leaf

extract :: Int -> Tree a -> [a]
extract i (Node (val, num) t1 t2 t3) =
    if num == i then [val] ++ (extract i t1) ++ (extract i t2) ++ (extract i t3)
    else (extract i t1) ++ (extract i t2) ++ (extract i t3)
extract i Leaf = []

-- game :: IO ()
-- game = do
--     while True (do num <- getLine)
    
--     putStr "Welcome to the Game!\nWhat secret number between 1 and 100 am I thinking of?!\n"
--     putStr "Enter guess number 1: "

while :: IO Bool -> IO () -> IO ()
while test action
  = do res <- test
       if res then do action
                      while test action
              else return ()

fa x = length x : x

fb [] ys = ys
fb (x:xs) ys = x : fb xs ys

fc (x:y:z) = x < y && fc (y:z)
fc _ = True

fd f xs = [f x | x <- xs]

punkt :: (b -> c) -> (a -> b) -> (a -> c)
punkt f1 f2 = \x -> f1 (f2 x)

data T a =
    L a |
    N a (T a) (T a)

transform :: T a -> (a -> a) -> T a
transform (L val) f  = (L (f val))
transform (N val t1 t2) f = (N (f val) (transform t1 f) (transform t2 f))

valid :: [[Integer]] -> (Int,Int) -> Bool
valid ((_:_):_) (1, 1) = True
valid ((r:rs):cs) (row, 1) = valid (rs:cs) ((row-1), 1)
valid (c:cs) (r, col) = valid cs (r, (col-1))
valid _ _ = False

m = [[8,8,8,8,8,8,8,8,8,8],[8,0,8,0,0,0,0,0,0,8],[8,0,8,0,8,8,0,8,8,8],[8,0,8,0,8,8,0,0,0,8],[8,0,0,0,0,0,0,8,0,8],[8,0,8,8,8,0,8,0,0,8],[8,0,8,0,8,0,8,0,8,8],[8,0,8,0,8,0,0,0,0,8],[8,8,8,8,8,8,8,1,8,8]]

a :: [[Integer]] -> (Int,Int) -> Integer
a ((r:rs):cs) (1, 1) = r
a ((r:rs):cs) (row, 1) = a (rs:cs) ((row-1), 1)
a (c:cs) (r, col) = a cs (r, (col-1))

escapable :: [[Integer]] -> (Int,Int) -> Bool
escapable ls (x,y) = ((valid ls (x,y)) && ((a ls (x,y)) == 1)) || (escapable ls ((x+1),y))
    || (escapable ls (x,(y+1))) || (escapable ls ((x-1),y)) || (escapable ls (x,(y-1)))
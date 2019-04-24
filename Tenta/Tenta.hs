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

game :: IO ()
game = do
    while True (do num <- getLine)
    
    putStr "Welcome to the Game!\nWhat secret number between 1 and 100 am I thinking of?!\n"
    putStr "Enter guess number 1: "

while :: IO Bool -> IO () -> IO ()
while test action
  = do res <- test
       if res then do action
                      while test action
              else return ()
type SubSeq = ([Int], Int, Int, Int)

takeKtoOne :: Int -> Int -> [Int] -> [SubSeq]
takeKtoOne s 1 (x : _) = [([x], x, s, s)]
takeKtoOne s k l = (seq, sum seq, s, s+k-1) : (takeKtoOne s (k-1) l)
    where seq = take k l

allSubSeq :: Int -> [Int] -> [SubSeq]
allSubSeq s (x : []) = [([x], x, s, s)]
allSubSeq s l = (takeKtoOne s (length l) l) ++ (allSubSeq (s+1) (tail l))

ins :: SubSeq -> [SubSeq] -> [SubSeq]
ins tup [] = [tup]
ins (xSeq, x, xF, xL) ((ySeq, y, yF, yL) : ys) = 
    if x <= y then 
        xTup : yTup : ys 
    else 
        yTup : ins xTup ys 
    where
        xTup = (xSeq, x, xF, xL)
        yTup = (ySeq, y, yF, yL)

iSort :: [SubSeq] -> [SubSeq]
iSort [] = []
iSort (x : xs) = ins x (iSort xs)

lineGen :: Int -> [SubSeq] -> String
lineGen _ [] = ""
lineGen 0 _ = ""
lineGen k ((seq, s, f, l) : rest) = (show s) ++ "\t" ++ (show f) ++ "\t" 
            ++ (show l) ++ "\t" ++ (show seq) ++ "\n" ++ (lineGen (k-1) rest)

strGen :: Int -> [Int] -> String
strGen k l = "Entire list: " ++ (show l) ++ "\n"
                ++ "size\ti\tj\tsublist\n" ++ (lineGen k (iSort (allSubSeq 1 l)))

-- Chapter 14
-- Ex 14.1
data Temp = Cold | Hot deriving (Eq, Ord, Show)
data Season = Spring | Summer | Autumn | Winter deriving (Eq, Ord, Show)

weather :: Season -> Temp
weather s = if s == Summer then Hot else Cold

-- Ex 14.4
data Shape = Circle Float 
            | Rectangle Float Float 
            | Triangle Float Float Float
            deriving (Eq,Ord,Show,Read)

perimiter :: Shape -> Float
perimiter (Circle r) = 2*r*pi
perimiter (Rectangle h w) = 2*h + 2*w
perimiter (Triangle a b c) = a+b+c

-- Ex 14.5
isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False 

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle h w) = h*w 
area (Triangle a b c) = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2
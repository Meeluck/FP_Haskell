fac :: Integer -> Integer
fac 0 = 0
fac x | x > 0 = x * fac (x-1)
fac _ = error "Negative argument in fac"

facT :: Integer -> Integer
facT x | x < 0 = error "Negative argument in fac"
facT x = fac' x 1
  where
    fac' 0 acc = acc
    fac' n acc = fac' (n-1) (acc*n)

-- Упражение 1    
-- без хвостовой рекурсии
fibN :: Integer -> Integer
fibN x | x < 0 = error "Negative argument in fibN"
fibN x | x == 0 = 0
fibN x | x == 1 = 1
fibN x = (fibN(x-1) + fibN(x-2));

-- с хвостовой рекурсией
fibN_Tail  :: Integer -> Integer
fibN_Tail x | x < 0 = error "Negative argument in fibN_Tail"
fibN_Tail x = fibN_Tail' x 0 1
    where
        fibN_Tail' 0 a b = a
        fibN_Tail' 1 a b = b
        fibN_Tail' x a b = fibN_Tail' (x - 1) b (a + b)


-- Упражнение 2
fibMap:: [Integer]
fibMap = map fibN_Tail [0,1..] 

fibZipWith :: [Integer]
fibZipWith = 0 : 1 : zipWith (+) fibZipWith (tail fibZipWith)

-- Упражнение 3
myDiv :: Integer -> Integer -> (Integer, Integer)
myDiv x 0 = error "Divide by zero"
myDiv 0 y = (0,0)
myDiv x y |  (x > 0 && y > 0) || (x < 0 && y < 0) = myDiv' (abs x) (abs y) 0
    where 
        myDiv' x y z |  x < y = (z,x)
        myDiv' x y z = myDiv' (x-y) y (z+1)
myDiv x y | x < 0 && y > 0 = myDiv'' x y 0
    where 
        myDiv'' x y z | (abs x) < y = (z,x)
        myDiv'' x y z = myDiv'' (x+y) y (z-1)
myDiv x y | x > 0 && y < 0 = myDiv''' x y 0
    where
        myDiv''' x y z | x < (abs y) = (z, (-x))
        myDiv''' x y z = myDiv''' (x - (abs y)) y (z-1)  
myDiv :: Integer -> Integer -> (Integer, Integer)
myDiv x 0 = error "Divide by zero"
myDiv 0 y = (0,0)
myDiv x y |  (x > 0 && y > 0) || (x < 0 && y < 0) = myDiv' x y 0
    where
        myDiv' x y z |  (abs x) < (abs y) = (z,x)
        myDiv' x y z = myDiv' (x-y) y (z+1)
myDiv x y | x < 0 && y > 0 = myDiv'' x y 0
    where
        myDiv'' x y z | (abs x) < y = (z,x)
        myDiv'' x y z = myDiv'' (x+y) y (z-1)
myDiv x y | x > 0 && y < 0 = myDiv''' x y 0
    where
        myDiv''' x y z | x < (abs y) = (z, x)
        myDiv''' x y z = myDiv''' (x - (abs y)) y (z-1)
-- Площадь круга короткий синтаксис
ariaCircle x =  3.14 * (x * x)
-- Площадь круга лямбда синтаксис
ariaCircleLambda = \x -> 3.14 * (x * x)

-- Исправление кода
area x = 3.14 * (x * x)
double x = x * 2
checkSum = let x = 7
               y = 10
               in x + y 

-- выполнение кода
-- 1) 5
-- 2) 25
-- 3) 30
-- 4) 8
-- 5) 300
-- 6) -17

five = x
    where x = 5
powFive = x
    where x = 5 * 5
xMultY = x * y
    where x = 5
          y = 6
t4 = x + 3
    where x = 5
          y = 1000
t5 = x * 5
    where x = 10 * 5 + y
          y = 10
t6 = z / x + y                  
    where x = 7
          y = negate x
          z = y * 10
myAbs x =
    if x >= 0 then x 
    else negate x

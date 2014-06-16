isEven :: Int -> Bool
isEven n = (mod n 2 == 0)

-- 1)
seven :: a -> Int
seven n = 7

signo :: (Ord a, Num a) => a -> Int
signo n | n < 0 = -1
        | n > 0 = 1
        | otherwise = 0

and' :: Bool -> Bool -> Bool
and' False y = False 
and' True y = y 

xor' x y | x == True && y == False = True
         | x == False && y == True = True
         | otherwise = False

or' :: Bool -> Bool -> Bool
or' True y = True
or' False y = y

not' :: Bool -> Bool
not' False = True
not' True = False

dividesTo x y = y `mod` x == 0

isCommonDivisor x (y,z) = (mod y x == 0) && (mod z x == 0)

--isCommonMult x (y,z) = 

-- 2)
-- Funcion a reescribir:
-- f1 x = let (y,z) = (x,x) in y
f1 x = x

greaterThan (x,y) | x > y = True
                  | otherwise = False

-- Funcion a reescribir:
-- f2 (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
f2 (x,y) = x

-- Funcion a redefinir:
-- power4 x = let sqr y = y*y in sqr (sqr x)
power4 x = x*x*x*x
power4' x = (sqr x)*(sqr x) where sqr x = x*x

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sauronico x | mod x 400 == 0 = True
            | otherwise = let a = mod x 100 in (mod a 4 == 0) && a /= 0

sort3 x y z | x < y && y < z = (x, y, z)
            | x < z && z < y = (x, z, y)
            | y < x && x < z = (y, x, z)
            | y < z && z < x = (y, z, x)
            | z < x && x < y = (z, x, y)
            | otherwise      = (z, y, x)

sort3' x y z = insert x (sort2 y z)
    where insert a (b,c) 
            | a <= b = (a, b, c)
            | a >= c = (b, c, a)
            | otherwise = (b, a, c)
          sort2 d e = if e <= d then (e, d) else (d, e)

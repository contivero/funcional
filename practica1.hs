-- 1)
seven :: a -> Int
seven n = 7

sign':: (Ord a, Num a) => a -> Int
sign' n | n < 0 = -1
        | n > 0 = 1
        | otherwise = 0

absolute :: (Ord a, Num a) => a -> a
absolute x | x >= 0 = x
           | x < 0 = (-x)

absolute' :: (Ord a, Num a) => a -> a
absolute' x | sign' x == -1 = (-x)
            | otherwise = x

and' :: Bool -> Bool -> Bool
and' False y = False
and' True y = y

xor' :: Bool -> Bool -> Bool
xor' x y | x == True && y == False = True
         | x == False && y == True = True
         | otherwise = False

or' :: Bool -> Bool -> Bool
or' True y = True
or' False y = y

not' :: Bool -> Bool
not' False = True
not' True = False

dividesTo :: Int -> Int -> Bool
dividesTo x y = y `mod` x == 0

isMultiple :: Int -> Int -> Bool
isMultiple x y = y `mod` x == 0 -- Igual al anterior ¿?

isCommonDivisor :: Int -> (Int, Int) -> Bool
isCommonDivisor x (y,z) = (x `mod` y == 0) && (x `mod` z == 0)

isCommonMult :: Int -> (Int, Int) -> Bool
isCommonMult x (y,z) = (y `mod` x == 0) && (z `mod` x == 0)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- 2)
-- Funcion a reescribir:
-- f1 x = let (y,z) = (x,x) in y
f1 x = x

greaterThan (x,y) | x > y = True
                  | otherwise = False

-- Funcion a reescribir:
-- f2 (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
f2 (x,y) = x

-- 3)
-- Redefinir:
--    power4 x = let sqr y = y*y in sqr (sqr x)
power4 x = x*x*x*x
power4' x = (sqr x)*(sqr x) where sqr x = x*x

-- 4)
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 5) Propiedades deseables en los programas:
-- Corrección (correctness) es la más importante. Después depende de
-- lo qué uno priorize. Algunos eficiencia, o también modificabilidad y
-- legibilidad (propiedades que tienen que ver con como lee el humano el
-- código más que con la ejecución).

-- 6) Funciones como valores y la aplicación parcial son las principales.

sauronico x | mod x 400 == 0 = True
            | otherwise = let a = mod x 100 in (mod a 4 == 0) && a /= 0

sort3 x y z | x < y && y < z = (x, y, z)
            | x < z && z < y = (x, z, y)
            | y < x && x < z = (y, x, z)
            | y < z && z < x = (y, z, x)
            | z < x && x < y = (z, x, y)
            | otherwise      = (z, y, x)

sort3' x y z = insert x (sort2 y z)
  where
    insert a (b,c)
      | a <= b = (a,b,c)
      | a >= c = (b,c,a)
      | otherwise = (b,a,c)
    sort2 d e = if e <= d 
                   then (e,d) 
                   else (d,e)

  sort3'' x y z = if x < y
                     then if y < z
                             then (x,y,z)
                             else if x > z
                                     then (z,x,y)
                                     else (x,z,y)
                     else if x > z
                             then if y < z
                                     then (y,z,x)
                                     else (z,y,x)
                             else if y,x,z

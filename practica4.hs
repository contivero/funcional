-- 1)
nextDiv :: Integer -> Integer -> Integer
nextDiv x y = if y `mod` h == 0 then h else nextDiv h y
    where h = x + 1

-- Suma divisores, tiene en cuenta el número en si y el 1!
sumDivs :: Integer -> Integer
sumDivs x = let sumDivRec a b = if a == 1 then a else if (b `mod` a == 0) then a + sumDivRec (a-1) b else sumDivRec (a-1) b in sumDivRec x x

power :: Float -> Integer -> Float
power 0 0 = error "undefined"
power _ 0 = 1
power x y = x * (power x (y-1))

dividesTo x y = if y == 0 then True else if y >= x then dividesTo x (y-x) else False

mySum f i j = if i == j then f i else f i + mySum f (i+1) j

-- Ver pq esta version mas eficiente usando la criba de eratóstenes falla!
--prime x = primeRec 2 x 
    --where primeRec a b = if a < floor (sqrt (fromInteger b)) then if (b `mod` a == 0) then False else primeRec (a+1) b else True

prime x = primeRec 2 x 
    where primeRec a b = if a < b then if (b `mod` a == 0) then False else primeRec (a+1) b else True

phi i = let phiRec a b = if prime b then if a == 1 then b else phiRec (a-1) (b+1) else phiRec a (b+1)
        in phiRec i 2 

-- 3)
-- * Tiene infinitos elementos
-- * Todos sus elementos o bien satisfacen una regla base, o satisfacen una regla inductiva
-- * Todos sus elementos son finitos
-- * El orden basado en "es parte de" es bien fundado (o sea, toda cadena
--   descendente es finita). Ej: Z es parte de SZ, SZ es parte de SSZ, etc.

-- 4)
--  Demostrar: flip (curry f) = curry (f . swap)
--
-- flip (curry f)
-- = principio de extensionalidad (2 veces)
--      flip (curry f) x y
-- = definición de flip
--      (curry f) y x
-- = definición de curry 
--      f (x,y)             (A)
--
--      curry (f . swap)
-- = principio de extensionalidad (2 veces)
--      curry (f . swap) x y
-- = definición de curry
--      (f . swap) (x,y)
-- = definición de composición (.)
--      f (swap(x,y))
-- = definición de swap
--      f (x,y)             (B) 
-- ==> (A) = (B)    ✓
--                       ∎
--------------------------------------------------
-- Demostrar: sum f i j + sum f (j+1) k = sum f i k
--
-- Por inducción en j-i:
-- Caso base: j-i = 0   ==>  j = i 
-- ==>  sum f i j + sum f (j+1) k = sum f (j+1) k 
--      sum f i j = f i   por definición de sum
-- ==>  f i + sum f (i+1) k = sum f i k 
--      dado que i < i+1 <= k ; por definición de suma
-- ==>  sum f i k = sum f i k      
--                              ✓
--
-- Hipótesis inductiva: j-i = n+1 ==> j - (i+1) = n
--      sum f (i+1) j + sum (j+1) k = sum f (i+k) k
-- Tesis inductiva: j-i = n
--      sum f i j + sum (j+1) k = sum i k
-- Demo:
--      sum f i j + sum (j+1) k =
--  = por definición de sum
--      sum (i+1) j + f i + sum (j+1) k =
--  = por HI
--      sum (i+1) k + f i
--  = por definición de sum
--      sum i k

-- 5)
hailstone n = if n <= 1
              then 0
              else if (n `mod` 2 == 0) then (n `div` 2)
                                       else (3 * n+1)

-- TODO puedo guardarme hailstone a en una variable para que repetir el calculo?
hail n = hailRec n 1 
    where hailRec a b = if hailstone a == 0 then b else hailRec (hailstone a) (b+1) 
-- ¿Para qué valores de la evaluación termina?
-- Se cree que para todos, pero aun no se ha demostrado (leer 'Conjetura de Collatz')

-- 6)
mcd x 0 = x
mcd x y = mcd y (x `mod` y)

-- Alternativamente
mcd' x y = if y == 0 then x else mcd' y (x `mod` y)

import Data.Char (ord, isAlpha) -- Usado para "codes" y "chars" del ejercicio 1
{-
Recordar !
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = if f x
                  then x:(filter f xs)
                  else filter f xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
-}

-- 1)

sum' = foldr (+) 0

any' = foldr (||) False

all' = foldr (&&) True

codes = foldr ((:) . ord) []

remainders n = foldr ((:) . (`mod` n)) []

squares = foldr ((:) . (\x -> x*x)) []

lengths = foldr ((:) . length) []

order = filter (\(x,y) -> x < 3*y)

pairs = filter (\x -> x `mod` 2 == 0)

chars = filter (isAlpha)

moreThan n = filter (\xs -> length xs > n)

-- 2) TODO ver si estan todos bien !

pal :: Eq a => [a] -> Bool
pal = reverse >>= (==) -- TODO: Magia monádica, ver bien que hace!

hs :: [[Char]] -> Int
hs = length . (filter (\x -> head x == 'h'))

--avgLength :: [a] -> Float
avgLength [] = 0
avgLength (x:xs) = fromIntegral (sum (lengths (x:xs))) / fromIntegral (length (x:xs))
-- avgLength = foldr (\x -> (sum (lengths x)) / (length (x))) 0

adjacent [] = []
adjacent [a] = []
adjacent (x:y:zs) = (x,y) : adjacent (y:zs)

diffAdj (x:xs) = filter (\(x,y) -> (x-y) `mod` 2 == 0) (adjacent (x:xs))

remDups [] = []
remDups [a] = [a]
remDups (x:xs) = if x == head xs
                    then remDups xs
                    else x:(remDups xs)

-- primes n =  VER COMO IMPORTAR PHI DE LA GUIA 4

-- 3)
-- f = id para las listas
f :: [a] -> [a]
f = foldr (:) []
{-
    f [1]
= definición de f
    foldr (:) [] [1]
= definición de foldr-2
    (:) 1 (foldr (:) [] [])
= definición de foldr-1
    (:) 1 []
= reescritura infija de (:)
    1:[]
= syntactic sugar
    [1]
-}
f' (x:xs) = (x:xs)

-- 4)
-- TOOD TERMINAR !! : filter' f (x:xs) = if f x then concat (map )map f (x:xs)

-- 5)
takewhile _ [] = []
takewhile f (x:xs) = if f x then x : (takewhile f xs) else []

dropwhile _ [] = []
dropwhile f (x:xs) = if f x then dropwhile f xs else (x:xs)

-- 6) TODO
-- ffreshIndex :: [Lt] -> Int

{-
7.a)
    map f (xs ++ ys) = map f xs ++ map f ys

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    map f ([] ++ ys)
= Por definición de (++).1
    map f ys                (A)

    map f [] ++ map f ys
= definición de map.1
    [] ++ map f ys
= definición de ++
    map f ys                (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = z:zs
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    map f ((z:zs) ++ ys) = map f (z:zs) ++ map f ys

Hipótesis inductiva: map f (zs ++ ys) = map f zs ++ map f ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    map f ((z:zs) ++ ys)
= definición de ++
    map f (z:(zs ++ ys))
= definición de map.2
    f z : (map f (zs ++ ys))
= por hipótesis inductiva
    f z : (map f zs ++ map f ys)
= definición de (++).2
    (f z : map f zs) ++ map f ys
= definición de map.2
    map f (z:zs) ++ map f ys
                                  ∎

7.b)
    map f . concat = concat . map (map f)
= por principio de extensionalidad
    (map f . concat) xss = (concat . map (map f)) xss

Caso base: xss = []
‾‾‾‾‾‾‾‾‾‾
    (map f . concat) []
= definición de composición
    (map f (concat []))
= definción de concat.1
    map f []
= definición de map.1
    []                          (A)

    (concat . map (map f)) []
= definición de composición
    (concat (map (map f) []))
= definición de map.1
    concat []
= definición de concat.1
    []                          (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xss = ys:yss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (map f . concat) (ys:yss) = (concat . map (map f)) (ys:yss)

Hipótesis inductiva: (map f . concat) yss = (concat . map (map f)) yss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    (map f . concat) (ys:yss)
= definición de composición
    map f (concat (ys:yss))
= definición de concat
    map f (ys ++ concat yss)
= propiedad demostrada en ej. 7.a)
    map f ys ++ map f (concat yss)
= definición de composición
    map f ys ++ ((map f . concat) yss)
= por hipótesis inductiva
    map f ys ++ ((concat . map (map f)) yss)
= definición de composición
    map f ys ++ concat (map (map f) yss)
= definición de concat.2
    concat ((map f) ys : map (map f) yss)
= definición de map.2
    concat (map (map f) (ys:yss))
= definición de composición
    (concat . map (map f)) (ys:yss)

7.c)
    filter p (xs ++ ys) = filter p xs ++ filter p ys
Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    filter p ([] ++ ys)
= definición de (++).2
    filter p ys                 (A)

    filter p [] ++ filter p ys
= definición de filter.1
    [] ++ filter p ys
= definición de (++).2
    filter p ys                 (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = z:zs
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    filter p ((z:zs) ++ ys) = filter p (z:zs) ++ filter p ys

Hipótesis inductiva: filter p (zs ++ ys) = filter p zs ++ filter p ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    filter p ((z:zs) ++ ys)
= definción de (++).2
    filter p (z :(zs ++ ys))

Subcaso f x = True:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
= definición de filter.2
    z:(filter p (zs ++ ys))
= por hipótesis inductiva
    z:(filter p zs ++ filter p ys)
= definición de (++).2
    (z:(filter p zs)) ++ filter p ys
= definición de filter.2
    filter p (z:zs) ++ filter p ys  ✓

Subcaso f x = False:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
= definición de filter.2
    filter p (zs ++ ys)
= por hipótesis inductiva
    filter p zs ++ filter p ys      (A)

Lado derecho:
    filter p (z:zs) ++ filter p ys
= definición de filter.2
    filter p zs ++ filter p ys      (B)

    (A) = (B)   ✓
                        ∎

7.d)
    map (map f) . map (x:) = map ((f x):) . map (map f)
= por principio de extensionalidad
    (map (map f) . map (x:)) xs = (map ((f x):)) . map (map f)) xs

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    (map (map f) . map (x:)) []
= definición de composición
    map (map f) (map (x:) [])
= definición de map.1
    map (map f) []
= definición de map.1
    []                          (A)

    (map ((f x):) . map (map f)) []
= definición de composición
    map ((f x):) (map (map f) [])
= definición de map.1
    map ((f x):) []
= definición de map.1
    []                          (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = ys:yss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (map (map f) . map (x:)) (ys:yss) = (map ((f x):)) . map (map f)) (ys:yss)

Hipótesis inductiva: (map (map f) . map (x:)) yss = (map ((f x):) . map (map f)) yss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    (map (map f) . map (x:)) (ys:yss)
= definición de composición
    map (map f) (map (x:) (ys:yss))
= definición de map.2
    map (map f) ((x:ys):(map (x:) yss))
= definición de map.2
    (map f (x:ys)) : (map (map f) (map (x:) yss))
= definición de composición
    (map f (x:ys)): ( (map (map f) . map (x:)) yss )
= por hipótesis inductiva
    (map f (x:ys)):((map ((f x):) . map (map f)) yss)       (A)

    (map ((f x):)) . map (map f)) (ys:yss)
= definición de composición
    map ((f x):) (map (map f) (ys:yss))
= definición de map.2
    map ((f x):) ((map f) ys : map (map f) yss)
= definición de map.2
    ((f x):) (map f ys) : (map ((f x):) (map (map f) yss))
= definición de composición
    ((f x): (map f ys)) : ((map ((f x):) . map (map f)) yss)
= definición de map.2
    (map f (x:ys)):((map ((f x):) . map (map f)) yss)       (B)

    (A) = (B)   ✓
                    ∎
7.e)
    concat . map concat = concat . concat
= por principio de extensionalidad
    (concat . map concat) xs = (concat . concat) xs

Caso base: xss = []
‾‾‾‾‾‾‾‾‾‾
    (concat . map concat) []
= definición de composición
    concat (map concat [])
= definición de map.1
    concat []
= definición de concat.1
    []                  (A)

    (concat . concat) []
= definición de composición
    concat (concat [])
= definición de concat.1
    concat []
= definición de concat.1
    []

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xss = (yss:ysss)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (concat . map concat) (yss:ysss) = (concat . concat) (yss:ysss)

Hipótesis inductiva: (concat . map concat) ysss = (concat . concat) ysss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    (concat . map concat) (yss:ysss)
= definición de composición
    concat (map concat (yss:ysss))
= definición de map.2
    concat (concat yss : map concat ysss)
= definición de concat.2
    concat yss ++ concat (map concat ysss)
= definición de composición
    concat yss ++ ((concat . map concat) ysss)
= por hipótesis inductiva
    concat yss ++ ((concat . concat) ysss)
= definición de composición
    concat yss ++ concat (concat ysss)
= definición de concat.2
    concat (concat yss : concat ysss)
=
--------------------------------------------
--      TODO
--      TERMINAR !!!
--      concat (concat (yss:ysss))
--
--      concat (yss ++ concat ysss)
--
--
--
-- concat xs:xss = xs ++ concat xss
---------------------------------------------

8)
insert y [] = []
insert y (x:xs) = if x < y then x:insert y xs else y:x:xs

evenPos [] = []
evenPos [x] = [x]
evenPos (x:y:xs) = x:evenPos xs
-- TODO averiguar bien !!
-- Creo que no se puede, recordar que foldr :: (a -> b -> b) -> b -> [a] -> b
-}

{-
9) TODO TERMINAR !!
inits :: [a] -> [[a]]
inits [] = []
inits (x:xs) = [x] : map (x:) (inits xs)


Demostrar:
    inits . map f = map (map f) . inits
= por principio de extensionalidad
    (inits . map f) xs = (map (map f) . inits) xs

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    (inits . map f) []
= definición de composición
    inits (map f [])
= definición de map.1
    inits []
= definición de inits.1
    []                      (A)

    (map (map f) . inits) []
= definición de composición
    map (map f) (inits [])
= definición de inits.1
    map (map f) []
= definición de map.1
    []                      (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = (ys:yss)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (inits . map f) (ys:yss) = (map (map f) . inits) (ys:yss)

Hipótesis inductiva: (inits . map f) yss = (map (map f) . inits) yss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    (inits . map f) (ys:yss)
= definición de composición
    inits (map f (ys:yss))
= definicion de map.2
    inits (f ys : map f yss)
= definición de inits.2
    [f ys] : map ((f ys):) (inits (map f yss))
= definición de composición
    [f ys] : map ((f ys):) ((inits . map f) yss)
= por hipótesis inductiva
    [f ys] : map ((f ys):) ((map (map f) . inits) yss)
= definición de composición
    [f ys] : map ((f ys):) ((map (map f) (inits yss)))
=
    [f ys] : map ((f ys):) ((map f) )
= definición de inits.2
    inits (f ys:)


-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs
--
inits :: [a] -> [[a]]
inits [] = []
inits (x:xs) = [x] : map (x:) (inits xs)

--      (map (map f) . inits) (ys:yss)
-- =
--      map (map f) (inits (ys:yss))
--
--      map (map f) ([ys] : map (ys:) (inits yss))
--
--      (map f) [ys] : map (map f) (map (ys:) (inits yss))
--
--      f [ys] : map (map (map f) (map (ys:) (inits yss)))
--
--
--      ???????????????????
--      ???????????????????
--      ?????? U_U ????????
--      ???????????????????
--      ???????????????????

10.a)
    map (f . g) = map f . map g
= por principio de extensionalidad
    map (f . g) xs = (map f . map g) xs

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    map (f . g) []
= definición de map.1
    []                      (A)

    (map f . map g) []
= definición de composición
    map f (map g [])
= definición de map.1
    map f []
= definición de map.1
    []                      (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = (ys:yss)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    map (f . g) (ys:yss) = (map f . map g) (ys:yss)

Hipótesis inductiva: map (f . g) yss = (map f . map g) yss
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    map (f . g) (ys:yss)
= definición de map.2
    (f . g) ys : map (f . g) yss
= hipótesis inductiva
    (f . g) ys : (map f . map g) yss
= definición de composición (2 veces)
    f (g ys) : map f (map g yss)
= definición de map.2
    map f (g ys : map g yss)
= definición de map.2
    map f (map g (ys:yss))
= definición de composición
    (map f . map g) (ys:yss)
                                  ∎

10.b)
    filter p . filter q = filter r where r x = p x && q x
= principio de extensionalidad
    (filter p . filter q) xs = (filter r where r x = p x && q x) xs

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    (filter p . filter q) []
= definición de composición
    filter p (filter q [])
= definición de filter.1
    filter p []
= definición de filter.1
    []                              (A)

    (filter r where r x = p x && q x) []
= definición de filter.1
    []                              (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Hipótesis inductiva: (filter p . filter q) ys = (filter r where r x = p x && q x) ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = (y:ys)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (filter p . filter q) (y:ys) = (filter r where r x = p x && q x) (y:ys)

Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    (filter p . filter q) (y:ys)
= definición de composición
    filter p (filter q (y:ys))
= definición de filter.2
    filter p (q y : filter q ys)

    ******TERMINAR*****
    ******TERMINAR*****
    ******TERMINAR*****

10.c)
    filter p . map f = map f . filter (p . f)
= principio de extensionalidad
    (filter p . map f) xs = (map f . filter (p . f)) xs

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    (filter p . map f) []
= definición de composoción
    filter p (map f [])
= definición de map.1
    filter p []
= definición de map.1
    []                              (A)

    (map f . filter (p . f)) []
= definición de composición
    map f (filter (p . f) [])
= definición de filter.1
    map f []
= definición de map.1
    []                              (B)

    (A) = (B)   ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = (y:ys)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (filter p . map f) (y:ys) = (map f . filter (p . f)) (y:ys)

Hipótesis inductiva: (filter p . map f) ys = (map f . filter (p . f)) ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
    (filter p . map f) (y:ys)
= definición de composición
    filter p (map f (y:ys))
= definición de map.2
    filter p (f y : map f ys)

Subcaso p (f y) = True:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
= definición de filter.2
    p (f y) : filter p (map f ys)
= definición decomposición
    p (f y) : (filter p . map f) ys
= hipótesis inductiva
    p (f y) : (map f . filter (p . f)) ys
= definición de composición (2 veces)
    (p .f) y : map f (filter (p . f) ys)
= definición de map.2
    map f ((p . f) y : filter (p . f) ys)
= condición del subcaso y definción de filter.2
    map f (filter (p . f) (y:ys))   ✓

Subcaso p (f y) = False:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
= definición de filter.2
    filter p (map f ys)
= definición de composición
    (filter p . map f) ys
= hipótesis inductiva
    (map f . filter (p . f)) ys         (A)

    (map f . filter (p . f)) (y:ys)
= definición de composición
    (map f (filter (p . f) (y:ys)))
= condición del subcaso y definición de filter.1
    map f (filter (p . f) ys)
= definición de composición
    (map f . filter (p . f)) ys         (B)

    (A) = (B)   ✓
                        ∎


10.d)
Demostrar:
    takewhile p xs ++ dropbwhile p xs = xs

Demostramos por inducción en la estructura de la lista xs:

Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    takewhile p [] ++ dropwhile p []
= definición de takewhile.1
    [] ++ dropwhile p []
= definición de (++).1
    dropwhile p []
= definición de dropwhile.1
    []          ✓

Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Hipótesis inductiva: takewhile p ys ++ dropbwhile p ys = ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = (y:ys)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    takewhile p (y:ys) ++ dropwhile p (y:ys) = (y:ys)

Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
Subcaso p y = True:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    takewhile p (y:ys) ++ dropwhile p (y:ys)
= condición del subcaso y definición de takewhile.2
    (y:(takewhile f ys)) ++ dropwhile p (y:ys)
= condición del subcaso y definición de dropwhile.2
    (y:(takewhile f ys)) ++ dropwhile p ys
= definición de (++).2
    y : (takewhile f ys ++ dropwhile p ys)
= hipótesis inductiva
    y:ys        ✓

Subcaso p y = False:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    takewhile p (y:ys) ++ dropwhile p (y:ys)
= condición del subcaso y definición de takewhile.2
    [] ++ dropwhile p (y:ys)
= definición de (++).1
    dropwhile p (y:ys)
= condición del subcaso y definición de dropwhile.2
    y:ys        ✓
                        ∎
-}

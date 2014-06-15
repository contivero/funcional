import Data.Char
-- 1) a_
-- 1_ and'  y  or' de la práctica 1
-- 2_ primero x y = x 
--    primero' x y = id x 
-- 3_ fun f g x = (f x, g x)
-- 4_ add x y = x + y
--    substract x y = x - y 
-- 5_ func f x = f x    siendo f cualquier funcion unaria de tipo Int -> Int (por ejemplo: factorial)
-- 6_ flip f x y = f y x 
--
-- b_ 
-- 1_ brokenAnd True x = x
--    brokenOr False x = x
-- 2_ 
-- 3_
-- 4_
-- 5_
-- 6_

-- 2)
-- www.haskell.org/haskellwiki/Currying
-- "In Haskell, all functions are considered curried: That is, all functions
-- in Haskell take just single arguments."
--
-- a_ Es de alto orden porque recibe una función.
--    fUno es igual a la función curry de Haskell (aunque de tipo
--    Int -> Int).
-- b_ Es de alto orden porque recibe una función.
-- c_ 1_ No es de alto orden ya que no recibe ni retorna una función.
--    2_ Se sigue comportando igual, pero aumento la expresividad 
type Pers = (Char, Char, Char)
fTres :: Pers -> Bool
fTres (c1, c2, c3) = c1 == c2 && c2 == c3

-- 3) ej3 = \y -> y ## x   

-- 4) TODO preguntar!

-- 5)
twice :: (a -> a) -> a -> a
twice f x = f (f x)
twice' :: (a -> a) -> a -> a
twice' = \f -> \x -> f (f x)

myflip :: (a -> a -> b) -> a -> a -> b
myflip f x y = f y x
myflip' :: (a -> a -> b) -> a -> a -> b
myflip' = \f -> \x -> \y -> f y x

inc :: Integer -> Integer
inc = (+1)
inc' :: Integer -> Integer
inc' = \x -> x+1

-- 6)
fix :: ((a -> b) -> a -> b) -> a -> b
fix f x = f (fix f) x

fork :: ((a -> b), (a -> c)) -> a -> (b,c)
fork (f,g) x = (f x,g x)
-- fork (fork, fork) (fork, fork)
-- (fork (fork, fork), fork (fork, fork))
-- fork (fork, fork) es una función que :: (a -> (b,c))
-- Entonces lo pedido tiene tipo ((a -> (b,c)), (a -> (b,c)) )
-- TODO !
-- Rta: ((a -> b, a -> c) -> (a -> (b,c), a -> (b, c)),
--       (a -> b, a -> c) -> (a -> (b,c), a -> (b, c)))

apply :: (a -> b) -> a -> b
apply f x = f x
-- TODO: me cuesta verlo !
-- apply apply apply :: (a -> b) -> a -> b

-- curry ya viene definida en ghci, y es:
-- curry :: ((a,b) -> c) -> a -> b -> c

-- 7) square (square (3+7))
--    square (3+7) * ______
--    (3+7) * (3+7) * square (3+7)
--    10 * (...)
--    10 * 10 ...
--    100 * ...
--
--    (3+7) * (3+7) * (3+7) * (3+7) 
--    10 * (3+7) * (3+7) * (3+7) 
--    10 * 10 * (3+7) * (3+7) 
--    100 * (3+7) * (3+7) 
--    100 * 10 * (3+7) 
--    1000 * (3+7) 
--    1000 * 10
--    10000

-- 8) No es una función valida.

-- 9)
undef x = 1 + undef x

-- 10) Ejercicio 1.4.1 del 
-- "Introduction to Functional Programming using Haskell" de Bird
-- h :: Integer -> Integer -> Integer
-- . :: (a -> b) -> (c -> a) -> (c -> b)
-- Por el tipo de la composicion (.), se ve que las afirmaciones a) y c)
-- son invalidas (la entrada de f es de tipo Integer, mientras que la salida
-- de g es de tipo (Integer -> Integer)).
-- La afirmación b) es verdadera:
-- h x y = f (g x y) ==> h x y = (f.(g x))y   Por definición de composición
--                   ==> h x = f . (g x)      Por extensionalidad

-- Recordar de la práctica 2 la función compose:
compose :: (a -> b) -> (c -> a) -> c -> b
compose f g = (\x -> f(g x))

sumDigit c x = compose (+x) digitToInt c 

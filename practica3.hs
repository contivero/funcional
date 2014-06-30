import Data.Char
{-
1.a)
1_ and' y or' de la práctica 1
2_ primero x y = x
   primero' x y = id x
3_ fun f g x = (f x, g x)
4_ add x y = x + y
   substract x y = x - y
5_ func f x = f x    siendo f cualquier funcion unaria de tipo Int -> Int (por ejemplo: factorial)
6_ flip f x y = f y x

1.b)
1_ brokenAnd True x = x
   brokenOr False x = x
2_
3_
4_
5_
6_

2) "In Haskell, all functions are considered curried: That is, all functions
   in Haskell take just single arguments."
   Fuente: www.haskell.org/haskellwiki/Currying

   De todas formas, depende de como uno lo interprete!

a_ Es de alto orden porque recibe una función.
   fUno es igual a la función curry de Haskell (aunque más restrictiva,
   de tipo Int -> Int). Esta currificada.
b_ Es de alto orden porque recibe una función. Esta currificada.
c.1_ No es de alto orden ya que no recibe ni retorna una función.
     Si está o no currificada depende de la interpretación. Técnicamente
     recibe una terna, desde ese punto de vista si, pero si uno lo toma
     como que recibe tres valores de tipo Char, hace algo con eso y retorna
     Bool, entonces no.
 .2_ Se sigue comportando igual, pero aumentó la expresividad. En este
     caso si esta currificada, ya que no se considera que forma a la persona
     (tipo Pers), sino que fTres recibe algo de tipo Pers.
-}

-- 3) Tomemos x = 5, y supongamos la siguiente definición de ## solo
--    para probar. Entonces ej3 = (##5)
a ## b = a*2 + b*3
ej3 = \y -> y ## 5

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
{-
fork (fork, fork) (fork, fork)
(fork (fork, fork), fork (fork, fork))
fork (fork, fork) es una función que :: (a -> (b,c))
Entonces lo pedido tiene tipo ((a -> (b,c)), (a -> (b,c)) )
TODO !
Rta: ((a -> b, a -> c) -> (a -> (b,c), a -> (b, c)),
      (a -> b, a -> c) -> (a -> (b,c), a -> (b, c)))
-}

apply :: (a -> b) -> a -> b
apply f x = f x
-- TODO: me cuesta verlo !
-- apply apply apply :: (a -> b) -> a -> b

-- curry ya viene definida en ghci, y es:
-- curry :: ((a,b) -> c) -> a -> b -> c

{-
7) Recordar que Haskell sabe que tiene que poner el mismo valor donde
se repiten variables, y de esa forma ahorra cálculos.
    square (square (3+7))
    square (3+7) * ...
    (3+7) * (...) * (...)
    10 * 10 * (...)
    100 * 100
    10000
-}

-- 8) No es una función valida.

-- 9)
undef x = 1 + undef x

{-
10) Ejercicio 1.4.1 del "Introduction to Functional Programming using Haskell"
de Bird.
h :: Integer -> Integer -> Integer
. :: (a -> b) -> (c -> a) -> (c -> b)
Por el tipo de la composicion (.), se ve que las afirmaciones a) y c)
son invalidas (la entrada de f es de tipo Integer, mientras que la salida
de g es de tipo (Integer -> Integer)).
La afirmación b) es verdadera:
h x y = f (g x y) ==> h x y = (f.(g x))y   Por definición de composición
                  ==> h x = f . (g x)      Por extensionalidad
-}

-- 11)
-- Recordar de la práctica 2 la función compose:
compose :: (a -> b) -> (c -> a) -> c -> b
compose f g = (\x -> f(g x))

sumDigit c x = compose (+x) digitToInt c

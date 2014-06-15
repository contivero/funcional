-- Ejercicio 1)
-- a_ Cualquier cosa que se evalue como true y false
-- True
-- False
-- b_ Funcion swap evaluada en una tupla
-- c)
constant :: Char -> Int
constant x = 10
-- d)
alwaysTrue :: (Int, Char) -> Bool
alwaysTrue (x,y) = True
alwaysFalse :: (Int, Char) -> Bool
alwaysFalse (x,y) = False 
-- e)
mapping :: (Int -> Int) -> Int
mapping x = 10
-- f)
func :: (Bool -> Bool, Int) -> (Bool -> Bool, Int)
func (x,y) = (x,y)
-- g) 
mapToBool :: a -> Bool
mapToBool x = True
mapToBool' x = False
-- h)
identity :: a -> a
identity x = x
-- 3)

-- 4)
first :: (a, b) -> a
first (x,y) = x

second :: (a, b) -> b
second (x,y) = y

const :: a -> b -> a
const x y = x

--compose :: (a -> b) -> (c -> a) -> (c -> b)
compose :: (a -> b) -> (c -> a) -> c -> b
compose f g = (\x -> f(g x))

apply :: (a -> b) -> a -> b
apply f x = f x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)

pairFunc :: (a-> b, c) -> d -> a -> (a -> b, (c, d), b) 
pairFunc (f1,f2) x y = (f1, (f2, x), (f1 y))

-- 5) A language is statically typed if the type of a variable is known at compile time. This in practice means that you as the programmer must specify what type each variable is. Example: Java, C, C++
-- The main advantage here is that all kinds of checking can be done by the compiler, and therefore a lot of stupid bugs are caught at a very early stage.

-- 6) 
-- a_ Bien formada. Rta: 16
-- b_ Mal formada, falta el else
-- c_ Depende? := No tiene un significado especifico en haskell,
--    se podria definir la funcion := y usarla.
-- d_ Mal formada. x no puede ser un numero y Bool al mismo tiempo
-- e_ Bien formada. Rta: False
-- f_ Mal formada, necesito separar ambos con &&

-- 7)
data ColorPrimario = Azul | Rojo | Amarillo deriving (Eq, Show)
data ColorSecundario = Color' ColorPrimario ColorPrimario deriving (Eq, Show)

mezclar :: ColorPrimario -> ColorPrimario -> ColorSecundario
mezclar x y | x == y = error "No mezclar el mismo color" 
mezclar x y = Color' x y 

data Punto = Punto Float Float deriving (Eq, Show)

modulo :: Punto -> Float
modulo (Punto x y) = sqrt $ x^2 + y^2

distanciaA :: Punto -> Punto -> Float
(Punto x1 y1) `distanciaA` (Punto x2 y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

xcoord :: Punto -> Float
xcoord (Punto x y) = x

ycoord :: Punto -> Float
ycoord (Punto x y) = y

suma :: Punto -> Punto -> Punto
suma (Punto x1 y1) (Punto x2 y2) = Punto (x1+x2) (y1+y2)

data Punto3D = Punto3D Float Float Float deriving (Eq, Show)

modulo' :: Punto3D -> Float
modulo' (Punto3D x y z) = sqrt $ x^2 + y^2 + z^2

distanciaA' :: Punto3D -> Punto3D -> Float
(Punto3D x1 y1 z1) `distanciaA'` (Punto3D x2 y2 z2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

xcoord' :: Punto3D -> Float
xcoord' (Punto3D x y z) = x

ycoord' :: Punto3D -> Float
ycoord' (Punto3D x y z) = y

suma' :: Punto3D -> Punto3D -> Punto3D
suma' (Punto3D x1 y1 z1) (Punto3D x2 y2 z2) = Punto3D (x1+x2) (y1+y2) (z1+z2)

data Figura = Circulo Punto Float | Rectangulo Punto Punto deriving (Eq, Show)

area :: Figura -> Float
area (Circulo _ r) = pi*r^2
area (Rectangulo (Punto x1 y1) (Punto x2 y2)) = (abs $ x2-x1) * (abs $ y2-y1)

perimetro :: Figura -> Float
perimetro (Circulo _ r) = 2*pi*r
perimetro (Rectangulo (Punto x1 y1) (Punto x2 y2)) = 2*base*altura
    where base = (abs $ x2-x1)
          altura = (abs $ y2-y1)

mover :: Figura -> Punto -> Figura
mover (Circulo (Punto x y) r) (Punto a b) = Circulo (Punto (x+a) (y+b)) r
mover (Rectangulo (Punto x1 y1) (Punto x2 y2)) (Punto a b) =
        Rectangulo (Punto (x1+a) (y1+b)) (Punto (x2+a) (y2+b))

--data Figura3D = Esfera Punto3D Float | Cubo Punto3D  Punto3D  Punto3D  Punto3D |
--                Elipsoide

-- 9)
-- id x tendria tipo de lo que fuera x, pero una vez que digo ese x ya estoy definiendo
-- cual es el tipo y deja de ser generico... como hago?

-- 10) Ejercicio 1.6.3 del "Introduction to Functional
-- Programming using Haskell" de Bird.
-- La función es invalida. El argumento de tom es una función (única forma
-- de que el retorno x x sea valido). Supongamos que x :: A -> B; x se aplica
-- a x, por lo que tendría que ser A = A -> B, pero no existe tal tipo A que
-- pueda cumplir con ello.

smaller (x,y,z) | x <= y && x <= z = x
                | y <= x && y <= z = y
                | z <= x && z <= y = z

-- smaller' = \x y z -> case () of 
--                        _ | x <= y && x <= z -> x
--                          | y <= x && y <= z -> y
--                          | z <= x && z <= y -> z
smaller' :: (Ord a) => (a,a,a) -> a
smaller' = \(x, y, z) -> if x <= y && x <= z then x else if y <= x && y <= z then y else z

-- El x a izquierda del = no es el mismo del x a la derecha. El scope es distinto.
scnd :: a -> b -> b
scnd x = \x -> x

scnd' :: a -> b -> b
scnd' = \x -> \y -> y

andThen :: Bool -> Bool -> Bool
andThen True y = y
andThen False y = False

andThen' :: Bool -> Bool -> Bool  
andThen' = \x -> \y -> if x == True then y else False

iff :: Bool -> Bool -> Bool
iff = \x -> \y -> if x then not y else y
iff' :: Bool -> Bool -> Bool
iff' x y = if x then not y else y

-- Notar el scope de las variables! La primera x es distinta a la 2da y 3ra,
-- que entre ellas son iguales
alpha :: a -> b -> b
alpha = \x -> \x -> x
alpha' :: a -> b -> b
alpha' x y = y

bhaskara :: Floating a => (a,a,a) -> (a,a)
bhaskara (a,b,c) = (-b + aux , -b - aux) 
    where aux = (sqrt $ b^2 - (4*a*c))/(2*a)

-- 14)
-- a_ No esta bien formada: Error de tipos
-- b_ No, no se puede hacer && entre Num y Bool: Error de tipo
-- c_ Esta bien formada. Rta: 4
-- d_ No, no se sabe si retorna Bool o Num: Error de tipo
-- e_ Qué es a? Qué b? Qué hago con el ; ? Error sintáctico 

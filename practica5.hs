import Data.Char (ord, isAlpha) -- Usado para "codes" y "chars" del ejercicio 2
{-
1)
a_ Válido
b_ Válido
c_ TODO: 'Parse error in pattern: n+1'
d_ Válido
e_ Válido
f_ " Conflicting definitions for 'a' "
g_ Idem anterior
h_ TODO: me lo toma al f ([]:[4]) = 1 (compila), pero al querer usarlo falla?
i_ Válido
k_ Válido, []:[] equivale a [[]]
-}

-- 2)
sum' [] = 0
sum' (x:xs) = x + sum xs

any' [] = False
any' (x:xs) = x || any' xs

all' [] = True
all' (x:xs) = x && all' xs

codes [] = []
codes (x:xs) = ord x : codes xs

remainders _ [] = []
remainders n (x:xs) = x `mod` n : remainders n xs

squares [] = []
squares (x:xs) = x*x : squares xs

lengths [] = []
lengths (xs:xss) = length xs : lengths xss

order [] = []
order ((x,y):xs) = if x < 3*y 
                      then (x,y): order xs 
                      else order xs

pairs [] = []
pairs (x:xs) = if x `mod` 2 == 0 
                  then x : pairs xs
                  else pairs xs

chars [] = []
chars (x:xs) = if isAlpha x 
                  then x : chars xs 
                  else chars xs

moreThan _ [] = []
moreThan n (xs:xss) = if length xs > n 
                         then xs : moreThan n xss 
                         else moreThan n xss

-- 3) TODO

-- 4) TODO
-- a_ xs debe ser una lista de listas, o []
-- b_

{-
5.a) 
    pairs . squares = squares . pairs
= por principio de extensionalidad
    (pairs . squares) xs = (squares . pairs) xs

Demostración por inducción estructural en xs
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    (pairs . squares) [] 
= Por definición de composición (.)
    pairs(squares [])
= Por squares-1
    pairs([])
= Por pairs-1
    []              (A)

    (squares . pairs) []
= Por definición de composición
    squares(pairs [])
= Por pairs-1
    squares([]) 
= Por squares-1
    []              (B)

    (A) = (B)   ✓


Caso inductivo:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Tesis inductiva: xs = y:ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (pairs . squares) y:ys = (squares . pairs) y:ys

Hipótesis inductiva: (pairs . squares) ys = (squares . pairs) ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
Subcaso y es par:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (pairs . squares) (y:ys)
= definición de composición (.)
    pairs (squares (y:ys))
= Por squares-2
    pairs( y*y : squares ys )
= Por pairs-2, sabiendo que y*y es par <==> y es par
    y*y : pairs (squares ys)
= definición de composición (.)
    y*y : (pairs . squares) ys
= Por hipótesis inductiva
    y*y : (squares . pairs) ys      (A)


    (squares . pairs) (y:ys)
= definición de composición (.)
    squares (pairs (y:ys))
= Por pairs-2, sabiendo que y*y es par <==> y es par
    squares (y : pairs ys))
= Por squares-2
    y*y : (squares (pairs ys))
= definición de composición
    y*y : (squares . pairs) ys      (B)

    (A) = (B)   ✓
    
Subcaso y es impar:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (pairs . squares) (y:ys)
= definición de composición (.)
    pairs (squares (y:ys))
= Por squares-2
    pairs( y*y : squares ys )
= Por pairs-2, sabiendo que y*y es impar <==> y es impar
    pairs (squares ys)
= definición de composición (.)
    (pairs . squares) ys
= Por hipótesis inductiva
    (squares . pairs) ys      (A)


    (squares . pairs) (y:ys)
= definición de composición (.)
    squares (pairs (y:ys))
= Por pairs-2, sabiendo que y*y es impar <==> y es impar
    squares (pairs ys))
= definición de composición
    (squares . pairs) ys      (B)

    (A) = (B)   ✓
                    ∎
-}

-- 5.b) TODO
{-
6) Demostrar:
    sum xs ≤ length xs * maxl xs
siendo maxl:
    maxl [] = 0
    maxl (x:xs) = x `max` maxl xs
Sabiendo que xs es una lista finita de números naturales

Demostración por inducción estructural en xs
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Caso base: xs = []
‾‾‾‾‾‾‾‾‾‾
    sum []
= Por sum-1
    0               (I)

    length [] * maxl []
= Por length-1
    0 * maxl []
= Por maxl-1
    0 * 0           (II)

    (I) = (II)   ✓


Tesis inductiva: xs = y:ys
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    sum (y:ys) ≤ length (y:ys) * maxl (y:ys) 

Hipótesis inductiva: sum ys ≤ length ys * maxl ys 
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Demostración:
‾‾‾‾‾‾‾‾‾‾‾‾‾
Lado izquierdo:
    sum (y:ys)
= Por sum-1
    y + sum ys
≤ Por hipótesis inductiva
    y + length ys * maxl ys             (A)

Lado derecho:
    length (y:ys) * maxl (y:ys)
= Por length-2
    (1 + length ys) * maxl (y:ys)
= Por maxl-2
    (1 + length ys) * (y `max` maxl ys)

Subcaso y > maxl ys:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (1 + length ys) * (y `max` maxl ys)
= Por definición de max
    (1 + length ys) * y
= Por distributividad del producto 
    y + length ys * y                   (B)

    (A) ≤ (B)   ✓

Subcaso y ≤ maxl ys:
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    (1 + length ys) * (y `max` maxl ys)
= Por definición de max
    (1 + length ys) * maxl ys
= Por distributividad del producto
    maxl ys + length ys * maxl ys       (C)

    (A) ≤ (C)   ✓
                    ∎
-}

{-
Lambda cálculo
7)
Para entender más, leer sobre 'Church booleans'.
-}

true_Lam = \x -> \y -> x
false_Lam = \x -> \y -> y
ifThenElse_Lam = \x -> x
-- Bajo esta definición, ifThenElse_Lam = \x -> \y -> \z -> x y z 
-- equivale a la dada!
not_Lam = \x -> ifThenElse_Lam x false_Lam true_Lam

and_Lam = \x -> \y -> ifThenElse_Lam x y false_Lam
or_Lam = \x -> \y -> ifThenElse_Lam x true_Lam y
xor_Lam = \x -> \y -> ifThenElse_Lam x (not_Lam y) y
iff_Lam = \x -> \y -> or_Lam (and_Lam x y) (and_Lam (not_Lam x) (not_Lam y))
-- Preguntar pq este xor es invalido:
-- xor_Lam = \x -> \y -> or_Lam (and_Lam (not_Lam x) y) (and_Lam x (not_Lam y))

-- 8)
pair_Lam = \x -> \y -> \z -> ifThenElse_Lam z x y
fst_Lam = \x -> x true_Lam 
snd_Lam = \x -> x false_Lam

-- 9)
-- b)
oddsIn [] = []
oddsIn (x:xs) = if x `mod` 2 == 1 
                   then x : oddsIn xs
                   else oddsIn xs

-- 10) TODO: completar
data DigBin = Cero | Uno
suma Cero x = x
suma x Cero = x
suma Uno Uno = Cero

carry Uno Uno = Uno
carry _ _ = Cero

sumaCarry Cero Cero Cero = (Cero, Cero)
sumaCarry Cero Cero Uno = (Uno, Cero)
sumaCarry Cero Uno Cero = (Uno, Cero)
sumaCarry Cero Uno Uno = (Cero, Uno)
sumaCarry Uno Cero Cero = (Uno, Cero)
sumaCarry Uno Cero Uno = (Cero, Uno)
sumaCarry Uno Uno Cero = (Cero, Uno)
sumaCarry Uno Uno Uno = (Uno, Uno)

prod Cero _ = Cero
prod _ Cero = Cero
prod Uno Uno = Uno

type NumBin = [DigBin]
sumabin xs ys = sumabin' xs ys Cero
sumabin' [] (x:xs) Cero = x:xs
sumabin' [] (x:xs) Uno = suma x Uno : sumabin' [] xs (carry x Uno)
sumabin' (x:xs) [] c = sumabin' [] (x:xs) c
sumabin' (x:xs) (y:ys) c = suma (suma x y) c : sumabin' xs ys (snd(sumaCarry x y c))

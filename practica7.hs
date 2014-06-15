-- 1)

sum' = 
-- sum' [] = 0
-- sum' (x:xs) = x + sum xs

-- any' [] = False
-- any' (x:xs) = x || any' xs
-- 
-- all' [] = True
-- all' (x:xs) = x && all' xs

-- codes [] = []
-- codes (x:xs) = ord x : codes xs
--
-- remainders _ [] = []
-- remainders n (x:xs) = x `mod` n : remainders n xs

-- squares [] = []
-- squares (x:xs) = x*x : squares xs
-- 
-- lengths [] = []
-- lengths (xs:xss) = length xs : lengths xss
-- 
-- order [] = []
-- order ((x,y):xs) = if x < 3*y then (x,y): order xs else order xs
-- 
-- pairs [] = []
-- pairs (x:xs) = if x `mod` 2 == 0 then x : pairs xs else pairs xs
-- 
-- chars [] = []
-- chars (x:xs) = if isAlpha x then x : chars xs else chars xs
-- 
-- moreThan _ [] = []
-- moreThan n (xs:xss) = if length xs > n then xs : moreThan n xss else moreThan n xss

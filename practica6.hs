-- 1)
belongs a [] = False
belongs a (x:xs) = a == x || belongs a xs

union xs [] = xs
union [] xs = xs
union (x:xs) (y:ys) = if belongs x (y:ys) 
                      then union xs (y:ys) 
                      else union xs (x:y:ys)

intersection xs [] = []
intersection [] xs = []
intersection (x:xs) (y:ys) = if belongs x (y:ys) 
                             then intersection xs (x:y:ys) 
                             else intersection xs (y:ys)

-- 2)
data TipTree a = Tip a | Join (TipTree a) (TipTree a)

heightTip (Tip a) = 0
heightTip (Join a b) = let c = heightTip a; d = heightTip b in if c > d then 1 + c else 1 + d

leaves (Tip a) = 1
leaves (Join a b) = leaves a + leaves b

nodes (Tip a) = 0
nodes (Join a b) = 1 + nodes a + nodes b

walkover (Tip a) = [a]
walkover (Join a b) = walkover a ++ walkover b

mirrorTip (Tip a) = Tip a
mirrorTip (Join a b) = Join (mirrorTip b) (mirrorTip a)

mapTip f (Tip a) = Tip (f a)
mapTip f (Join a b) = Join (mapTip f a) (mapTip f b)

-- 4)
data Seq a = Nil | Unit a | Cat (Seq a) (Seq a)

appSeq Nil b = b
appSeq a Nil = a
appSeq a b = Cat a b


conSeq a b = appSeq (Unit a) b

lenSeq Nil = 0
lenSeq (Unit a) = 1
lenSeq (Cat a b) = 1 + (lenSeq a) + (lenSeq b)

revSeq Nil = Nil
revSeq (Unit a) = (Unit a)
revSeq (Cat a b) = Cat (revSeq b) (revSeq a)

headSeq (Cat a b) = headSeq a
headSeq a = a
--headSeq Nil = Nil
--headSeq (Unit a) = (Unit a)

tailSeq (Cat a b) = tailSeq b
tailSeq a = a

normSeq Nil = Nil
normSeq (Unit a) = Unit a
normSeq (Cat Nil Nil) = Nil
normSeq (Cat Nil (Unit b)) = Unit b
normSeq (Cat (Unit a) Nil) = Unit a
normSeq (Cat a b) = normSeq (Cat (normSeq a) (normSeq b)) -- Esta bien esto??

eqSeq Nil Nil = True
eqSeq Nil _ = False
eqSeq _ Nil = False
eqSeq (Unit a) (Unit b) = a == b
eqSeq (Unit a) (Cat b c) = False
eqSeq (Cat a b) (Unit c) = False
eqSeq (Cat a b) (Cat c d) = eqSeq a c && eqSeq b d

seq2List Nil = []
seq2List (Unit a) = [a]
seq2List (Cat a b) = seq2List a ++ seq2List b

-- 5)
data Form = Atom 
          | Or Form Form
          | Implies Form Form
          | Forall Var Form 
          | Not Form 
          | And Form Form
          | Iff Form Form
          | Exists Var Form

--normalize :: Form -> Form

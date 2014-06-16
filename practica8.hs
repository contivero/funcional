{-
1.a)
-}
isBalanced s = isBalancedRec s == 0
    where isBalancedRec emptyS = 0
          isBalancedRec (pushS e s) = if e == '('
                                     then 1 + isBalancedRec s 
                                     else if e == ')'
                                          then isBalancedRec s - 1
                                          else isBalancedRec s

isBalanced s = isBalancedRec s == 0
    where isBalancedRec emptyS = 0
          isBalancedRec (pushS e s) = if e == '['
                                     then 1 + isBalancedRec s 
                                     else if e == ']'
                                          then isBalancedRec s - 1
                                          else isBalancedRec s

isBalanced c s = isBalancedRec c s == 0
    where isBalancedRec _ emptyS = 0
          isBalancedRec c d (pushS e s) = if e == c
                                          then 1 + isBalancedRec c d s 
                                          else if e == d 
                                               then isBalancedRec c d s - 1
                                               else isBalancedRec c d s

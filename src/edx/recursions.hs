replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

(!!!) :: [a] -> Int -> a

(!!!) (x:xs) 1 = x
(!!!) (x:xs) n =  (!!!) xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = if e == x then True else elem' e xs 

drop' :: Int -> [a ] -> [a ]
drop' 0 xs = xs
drop' n [] = []
drop' n ( _ : xs) = drop' (n-1) xs

{-
Choose the correct definition for the function 
merge :: Ord a => [a] -> [a] -> [a]
 that merges two sorted lists in ascending order to give a single sorted list in ascending order.
  For example:

> merge [2, 5, 6] [1, 3, 4]
[1, 2, 3, 4, 5, 6]  
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = merge smaller_xs smaller_ys ++ [lesser,greater] ++ merge larger
	where 	greater = if x > y then x else y 
			lesser = if x < y then x else y
			smaller = [a|a<-]

--EXERCISE 10  (1 point possible)
{-
Choose the correct definition for the function 
msort :: Ord a => [a] -> [a] 
that implements merge sort, in which the empty list and singleton lists are already sorted,
 and any other list is sorted by merging together the two lists that result from sorting 
 the two halves of the list separately. 
 The solutions can use the function merge from the previous exercise 
 and the function halve that splits a list into two halves whose lengths differ by at most one.
-}

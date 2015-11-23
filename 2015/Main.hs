module Main where

import Prelude hiding ((!!), elem)

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x : xs)
    | e == x = True
    | otherwise = elem e xs

-- Choose the correct definition for the function merge
-- that merges two sorted lists in ascending order to give a single sorted list in ascending order.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Choose the correct definition for the function msort that implements merge sort,
-- in which the empty list and singleton lists are already sorted,
-- and any other list is sorted by merging together the two lists that result from sorting the two halves
-- of the list separately.
-- The solutions can use the function merge from the previous exercise and the function halve that splits a list
-- into two halves whose lengths differ by at most one.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs1) (msort xs2)
    where (xs1 , xs2) = halve xs

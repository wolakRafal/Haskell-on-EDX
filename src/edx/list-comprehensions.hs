-- EXERCISE 2  (1 point possible)
{-
   A triple (x, y, z) of positive integers is pythagorean if x2+y2=z2. Choose the correct implementation for the function

   pyths :: Int -> [(Int, Int, Int)]

   that returns the list of all pythagorean triples whose components are at most a given limit.

   > pyths 10
   [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
-}
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a,b,c) | a <- [1..n] , b <- [1..n] , c <- [1..n], a^2 + b^2 == c^2]

--EXERCISE 3  (1 point possible)
{-
A positive integer is perfect if it equals the sum of its factors, excluding the number itself. 
Wtite the correct definition of the function 
perfects :: Int -> [Int] that
 returns the list of all perfect numbers up to a given limit.

> perfects 500
[6, 28, 496]
-}
perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], isPerfect x ]
	where	
			factors num = [x | x <- [1..num], num `mod` x == 0]
			isPerfect num = num == sum (init (factors num))

-- EXERCISE 5  (1 point possible)
{-
	Redefine the function positions discussed in the lecture, using the function find:  
find :: (Eq a) => a -> [(a,b)]-> [b]
find k t = [v | (k',v) <- t, k == k']
-}

find :: (Eq a) => a -> [(a,b)]-> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [1..])

-- EXERCISE 6  (1 point possible)
{- The scalar product of two lists of integers xs and ys of length n 
 is given by the sum of the products of corresponding integers:

sum ( (xs !! i) * (ys !! i) ) for i = 0 to n-1
Choose the correct definition of scalarproduct :: [ Int ] -> [ Int ] -> Int that returns the scalar product of two lists.

> scalarproduct [1, 2, 3] [4, 5, 6]
32
-}
scalarproduct xs ys = sum [ x * y | (x, y) <- xs `zip` ys]

-- EXERCISE 12
{-hoose the correct definition of the function 
riffle :: [a] -> [a] -> [a] 
that takes two lists of the same length and interleaves their elements in turn about order.

For example:
riffle [1,2,3] [4,5,6] = [1,4,2,5,3,6]
-}
riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [ [x,y] | (x,y) <- xs `zip` ys]

-- EXERCISE 13  (1 point possible)
{-Choose the correct definition for the function 

divisors :: Int -> [Int] 
that returns the divisors of a natural number.

For example:
divisors 15 = [1, 3, 5, 15]

The function 
divides :: Int -> Int -> Bool 
decides if one integer is divisible by another. (Note: You need to implement this function yourself.)

For example:

divides 15 2 = False
divides 15 3 = True
-}

divisors :: Int -> [Int]
divides :: Int -> Int -> Bool

divides x y = y `mod` x == 0
divisors n = [x | x <- [1..n], x `divides` n ]

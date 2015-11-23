module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [x| x <- xs, even x ]
--show (evens [2, 5, 6, 13, 32] )
answer0 = sum . evens $ [827305 .. 927104]
--43772529500
answer1 = sum . evens $ []

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer] 
squares n = [x*x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)
answer4 = sumSquares 50

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: Integer -> Integer -> [Integer]
squares' m n = [x*x | x <- [(n+1)..(m+n)]] 

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)
answer5 = sumSquares' 50
answer6 = sum $ squares' 10 0
answer7 = sum $ squares' 0 10

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]
answer8 = foldr (-) 0 . map (uncurry (*)) $ coords 5 7

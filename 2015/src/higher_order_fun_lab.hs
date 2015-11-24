evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

-- squares 4 = [1*1, 2*2, 3*3, 4*4]
squares :: Integer -> [Integer]
squares n = [x*x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

squares' m n = [x*x | x <- [(n+1)..(n+m)]]
sumSquares' x = sum . uncurry squares' $ (x, x)

-- [0..m] × [0..n]
--coords 1 1 = [(0,0), (0,1), (1,0), (1,1)]
--coords 1 2 = [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
coords :: Integer -> Integer -> [(Integer, Integer)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]


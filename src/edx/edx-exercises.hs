-- Chapter 2
n    = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

last' [] = error "Empty list have no last element"
last' [x] = x
last' (x:xs) = last' xs

--The library function init, which removes the last element from a non-empty list, 
-- can be defined in terms of the library functions introduced in this chapter. 
-- recursive
init' [x] = []
init' (x:xs) = x : init' xs
-- with built in functions

init1 xs = take (length xs - 1) xs

-- Define a function product that produces the product of a list of numbers, 
-- and show using your definition that product [2, 3, 4] = 24.
product' [] = 1
product' (x:xs) = x * product xs

-- Functions

add 	:: (Int,Int) -> Int
add (x ,y) = x+y

-- this is the same:
add'	:: Int -> (Int->Int)
add' x y = x+y
zeroto :: Int -> [Int]
zeroto n = [0..n]

mult :: (Int, Int, Int) -> Int
mult1 :: Int -> (Int -> (Int -> Int))
mult2 :: Int -> Int -> Int -> Int

mult (x, y, z) = x*y*z
mult1 x y z = x*y*z
mult2 x y z = x*y*z

-- Edx Week3 / Hutton Chapter 4
odds	::	Int -> [Int]
odds n 	= map f [0..n-1]
			where f x = x*2+1

-- halve
halve :: [a] -> ([a],[a])
halve = \xs -> ([x | x <- take ((length xs) `quot` 2) xs] , [x | x <- drop ((length xs) `quot` 2) xs])

-- using where
halve2 :: [a] -> ([a],[a])
halve2 xs = (left,right)
		where 
			left = [x | x <- take ((length xs) `quot` 2) xs]
			right = [x | x <- drop ((length xs) `quot` 2) xs]
halve3 :: [a] -> ([a],[a])
halve3 xs = (take n xs, drop n xs)
			where n = length xs `div` 2

halve4 xs = splitAt (length xs `div` 2) xs

halve5 xs = (take n xs, drop n xs)
		where n = length xs `div` 2

-- savetail
safetail :: [a] -> [a]
safetail [] = []
safetail (_:tail) = tail

safetail2 [] = []
safetail2 xs = tail xs 

safetail3 :: [a] -> [a]
safetail3 xs 	
	| null xs = []
	| otherwise = tail xs

safetail4 xs = if null xs then [] else tail xs

safetail5 
	= \ xs ->
		case xs of
			[] -> []
			(_:xs) -> xs
-- Conjunction opertor ||
False || False = False
 _ || _ = True

 b || c
 	| b == c = True
 	| otherwise = False
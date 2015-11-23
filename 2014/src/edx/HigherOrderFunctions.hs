--Choose all options that implement the Prelude function all

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

all1 p = and . map p
all2 p = not . any (not . p)
all3 p xs = foldl (&&) True (map p xs)
all4 p = foldr (&&) True . map p 

-- Choose all options that implement the Prelude function any
any' :: (a -> Bool) -> [a] -> Bool

any' p = or . map p
any1 p xs = length (filter p xs) > 0
any2 p = not . null . dropWhile (not . p)
any3 p xs = not ( all (\ x -> not (p x)) xs)
any4 p xs = foldr (\ x acc -> (p x) || acc) False xs

-- Choose the option that implements the Prelude function takeWhile
takeWhile' :: ( a-> Bool) -> [a] ->[a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
	| p x = x : takeWhile p xs
	| otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
	| p x = dropWhile p xs
	| otherwise = x : xs



-- Choose the option that implements the Prelude function map
map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\ xs x -> xs ++ [f x]) []
map1 f = foldr (\ x xs -> f x : xs) []
map2 f = foldl (\xs x -> xs ++ [f x]) []

filterl  :: (a ->Bool) -> [a] -> [a]

-- foldl
filterl p = foldl (\xs x -> if p x then xs ++ [x] else xs) []
--foldr
filterr p = foldr (\x xs -> if p x then x : xs else xs) []


-- EXERCISE 7  (1 point possible)
{-Choose a definition for the function 

dec2int :: [Integer] -> Integer 

that converts a finite, non-partial list of non-bottom Integer digits, that represents a decimal number, 
into the non-bottom Integer this list represents. 

For example:
> dec2int [2, 3, 4, 5]
2345
> dec2int []
0
> dec2int [0, 0, 0, 0]
0
-}
dec2int :: [Integer] -> Integer

-- foldl
dec2int (n:ns) = foldl (\acc x -> acc*10 + x) n ns
-- foldr
dec2intr = foldr (\x acc -> acc*10 + x) 0  -- invalid

-- EXERCISE 9  (1 point possible)
{-Choose the correct definition for the Prelude function 
curry :: ((a, b) -> c) -> a -> b -> c ,
 that converts a function that takes its arguments as a pair into a function that takes its arguments one at a time. 
 For this exercise assume that bottom does not exist.-}

curry :: ((a, b) -> c) -> a -> b -> c
curry f = (\x y -> f (x, y))

 -- Choose the definition for the Prelude function uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x,y) -> f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

int2bin = unfold (==0) (`mod` 2) (`div` 2)
{-
Next consider the function chop8 :: [Bit] -> [[Bit]] 
that takes a list of bits and chops it into lists of at most eight
 bits (assuming the list is finite, non-partial,
  and does not contain bottom):-}
type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)
chop8' = unfold null (take 8) (drop 8)

-- EXERCISE 12
--choose an implementation of map :: (a -> b) -> [a] -> [b] using unfold.
mapUnfold f = unfold null (f . head) tail

--EXERCISE 13  (1 point possible)
--Choose an implementation of the Prelude function iterate :: (a -> a) -> a -> [a] using unfold.
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f 
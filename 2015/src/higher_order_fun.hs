--takeWhile :: (a -> Bool) -> [a] -> [a]

--takeWhile _ [] = []
--takeWhile p (x : xs)
--	| p x = x : takeWhile p xs
--	| otherwise = []

--dopWhile :: (a-> Bool) -> [a] -> [a]
--dopWhile _ [] = []
--dopWhile p (x : xs)
--	| p x = dropWhile p xs
--	| otherwise = x : xs

--map :: (a -> b) -> [a] -> [b]

----using foldr
--map f = foldr (\ x acc -> f x : acc) []

----using foldl
--map f = foldl (\ acc x -> acc ++ [f x]) []

--dec2int :: [Integer] -> Integer

--dec2int = foldl (\ acc x -> acc*10 + x) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- this not even typecheck:
sumsqr = compose [sum, map (^2), filter even]

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

type Bit = Int

-- Consider the function chop8 :: [Bit] -> [[Bit]] 
-- that takes a list of bits and chops it into lists of at most eight bits 
-- (assuming the list is finite, non-partial, and does not contain bottom):
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- implementation of chop8 using unfold:
chop8' = unfold null (take 8) (drop 8)


--  implementation of map :: (a -> b) -> [a] -> [b] using unfold:
map :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail 

--iterate using unfold.
iterate :: (a -> a) -> a -> [a] 
iterate f = unfold (const False) id (f x)

-- Church Numerals, 
-- what could be a possible implementation for exponentiation? 
cExp :: CNat -> CNat -> CNat
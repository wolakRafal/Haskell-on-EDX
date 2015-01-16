putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

getLine' = get ""

get :: String -> IO String

get xs = do 
			x <- getChar
			case x of
				'\n' -> return xs
				_ -> get (xs ++ [x])

{-EXERCISE 4
Which of the following implementations defines a function 

	interact' :: (String -> String) -> IO () 

that takes as its argument a function of type String -> String, 
and reads a line from the standard input, and passes it to this function, 
and then prints the resulting output followed by a newline on the standard output?
-}
interact' :: (String -> String) -> IO ()
interact' f = do
				line <- getLine'
				putStrLn' (f line)
				
{-EXERCISE 5
Choose all possible implementations of the function 

sequence_' :: Monad m => [m a] -> m () 

that takes a finite, non-partial, list of non-bottom, monadic values, 
and evaluates them in sequence, from left to right, ignoring all (intermediate) results?-}
sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
--sequence_' (m:ms) = m >>= \_ -> sequence_' ms
--sequence_' (m:ms) = m >> sequence_' ms 
--sequence_' (m:ms) = (foldl (>>) m ms) >> return () 
sequence_' ms = foldr (>>) (return()) ms


{- EXERCISE 6
Choose all possible implementations of the function 

sequence' :: Monad m => [m a] -> m [a] 

that takes a finite, non-partial, list of non-bottom, monadic values, 
and evaluates them in sequence, from left to right, 
collecting all (intermediate) results into a list
-}
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = m >>= \a -> do 
								as <- sequence' ms
								return (a:as)

sequence2 :: Monad m => [m a] -> m [a]
sequence2 [] = return []
sequence2 ms = foldr func (return []) ms
	where
		func :: (Monad m) => m a -> m [a] -> m [a]
		func m acc = do 
						x <- m
						xs <- acc
						return (x:xs)

sequence3 :: Monad m => [m a] -> m [a]
sequence3 [] = return []
sequence3 (m:ms) = do
					a <- m
					as <- sequence3 ms
					return (a:as)


-- EXERCISE 7  (1 point possible)
{-Choose all possible implementations of a function 

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]

 which takes a non-bottom function of type a -> m b, and a finite, 
 non-partial list of non-bottom elements of type a and (similarly to map)
  applies the function to every element of the list, but produces the resulting list
   wrapped inside a monadic action?

Note: mapM' must preserve the order of the elements of the input list.

Hint: Make sure you try each of these options in 
GHCi and play around with a variety of inputs to experiment with the behaviour. 
It's easiest to use the IO Monad for the function passed into mapM'
-}
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a:as) = f a >>= \b -> mapM' f as >>= \bs -> return (b:bs)

mapM2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2 f [] = return []
mapM2 f (a:as) = do
					b <- f a
					bs <- mapM2 f as
					return (b:bs)

mapM3 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM3 f [] = return []
mapM3 f (a:as) = f a >>= \b -> do
								bs <- mapM3 f as
								return (b:bs)

{-Which of the following definitions implements the function 

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]

, that takes a "predicate" of type Monad m => a -> m Bool and uses this to filter a finite,
 non-partial list of non-bottom elements of type a.-}

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do 
						flag <- p x
						ys <- filterM' p xs
						if flag then return (x:ys) else return ys

--define a control structure that repeats an action until it returns the 'False' result:
while' :: IO Bool -> IO ()
while' action = do 
	v <- action
	if v then while' action else return ()


--EXERCISE 9  (1 point possible)
--Implement the function 

--foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a 

--that takes an accumulation function a -> b -> m a, and a seed of type a and left folds a finite,
-- non-partial list of non-bottom elements of type b into a single result of type m a

--Hint: The recursive structure of foldLeftM looks as follows:

--foldLeftM f a [] = ... a ...
--foldLeftM f a (x:xs) = foldLeftM f (... f ... a ... (>>=) ...) xs 
--Remember the definition of foldl:

--foldl f a [] = ... a ...
--foldl f a (x:xs) = foldl f (... f ... a ...) xs 

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a 
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = do
	acc <- (f a x)
	foldLeftM f acc xs
-- reselut of expression
foldLeftEx9Ressult = foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r 

foldl' :: (a->b->a) -> a -> [b] -> a
foldl' f a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs 

foldr' ::  (a->b->b) -> b-> [a] -> b
foldr' f a [] = a
foldr' f a (x:xs) = foldr' f (f x a) xs 

--EXERCISE 10  (1 point possible)
--Implement the function 

--foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b 

--which is like to foldLeftM from the previous exercise, 
--except that it folds a finite, non-partial list of non-bottom elements of type a into 
--a single monadic value of type m b.

--Hint: look up the definition of foldr.

--What is the result of evaluating the expression:
--foldRightM (\a b -> putChar a >> return (a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f a [] = return a
foldRightM f a (x:xs) = do
	acc <- foldRightM f a xs
	f x acc
	

foldRightMEx10Result = foldRightM (\a b -> putChar a >> return (a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r
-- EX 11
{-Choose all possible implementations that define a function 

liftM :: Monad m => (a -> b) -> m a -> m b 

that takes a function of type a -> b and "maps" it over a non-bottom 
monadic value of type m a to produce a value of type m b?
-}
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = do 
	a <- m
	return (f a) 

liftM' f m = m >>= \a -> return (f a) 
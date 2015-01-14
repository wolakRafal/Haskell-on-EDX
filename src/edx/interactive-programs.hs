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
sequence' (m:ms)
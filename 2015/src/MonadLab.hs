module MonadLab where

putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

-- all possible implementation of putStrLn
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""


putStrLn2 [] = putChar '\n'
putStrLn2 xs = putStr' xs >> putStrLn2 ""


putStrLn3    :: String -> IO ()
putStrLn3 [] = putChar '\n'
putStrLn3 xs = putStr' xs >>= \x -> putChar '\n'

putStrLn4 [] = putChar '\n'
putStrLn4 xs = putStr' xs >> putStr' "\n"


-- sequence
sequence_'          :: Monad m => [m a] -> m ()
sequence_' []       = return ()
sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

sequence_3 ms = foldl (>>) (return ()) ms
sequence_4 ms = foldr (>>) (return ()) ms

sequence_2 []       = return ()
sequence_2 (m : ms) = m >> sequence_2 ms

sequence'           :: Monad m => [m a] -> m [a]
sequence' []        =  return []
sequence' (m : ms)  = m >>= \a ->
                                do as <- sequence' ms
                                   return (a : as)

sequence3 []        =  return []
sequence3 (m : ms)  =  do a <- m
                          as <- sequence3 ms
                          return (a : as)

sequence2 ms        = foldr func (return []) ms
                        where
                          func :: (Monad m) => m a -> m [a] -> m [a]
                          func m acc = do x <- m
                                          xs <- acc
                                          return (x : xs)
-- akes a non-bottom function of type a -> m b, and a finite, non-partial list of non-bottom elements of type a
-- and (similarly to map) applies the function to every element of the list, but produces the resulting list
-- wrapped inside a monadic action
-- Note:mapM' must preserve the order of the elements of the input list.
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
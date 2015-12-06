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
-- Takes a non-bottom function of type a -> m b, and a finite, non-partial list of non-bottom elements of type a
-- and (similarly to map) applies the function to every element of the list, but produces the resulting list
-- wrapped inside a monadic action
-- Note:mapM' must preserve the order of the elements of the input list.
foo prompt = do putStr' prompt
                x <- getLine
                return x

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

mapM2 f [] = return []
mapM2 f (a : as) = f a >>= \b -> mapM' f as >>= \bs -> return (b : bs)

mapM3 f [] = return []
mapM3 f (a : as) = do b <- f a
                      bs <- mapM3 f as
                      return (b : bs)

mapM4 f [] = return []
mapM4 f (a : as) = f a >>=
                    \b -> do
                            bs <- mapM4 f as
                            return (b : bs)




-- The following definitions implements the function
-- filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a],
-- that takes a "predicate" of type Monad m => a -> m Bool
-- and uses this to filter a finite, non-partial list of non-bottom elements of type a.
--          Note: filterM' must preserve the order of the elements of
--                the input list, as far as they appear in the result.

ask t = do putStr' ("Is upper: " ++ t)
           res <- getLine
           if (res == "y") then return True else return False

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ []       = return []
filterM' p (x : xs) = do flag <- p x
                         ys <- filterM' p xs
                         if flag then return (x : ys) else return ys



--Function that takes an accumulation function a -> b -> m a,
-- and a seed of type a and left folds a finite,
-- non-partial list of non-bottom elements of type b into a single result of type m a
foldLeftM            :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM _ a []     = return a
foldLeftM f a (b:bs) = do aa <- f a b
                          foldLeftM f aa bs

-- > foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r
-- result: haskelllleksahhaskell


-- Hint: The recursive structure of foldLeftM looks as follows:
--
-- foldLeftM f a [] = ... a ...
-- foldLeftM f a (x:xs) = foldLeftM f (... f ... a ... (>>=) ...) xs
-- Remember the definition of foldl:
--
-- foldl f a [] = ... a ...
-- foldl f a (x:xs) = foldl f (... f ... a ...) xs
-- What is the result of evaluating the expression:

-- implementation of foldRightM
foldRightM              :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM _ b []       = return b
foldRightM f b (a : as) = do bb <- foldRightM f b as
                             f a bb

-- > foldRightM (\a b -> putChar a >> return(a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r
-- result : ]9,7,5,3,1[[1,3,5,7,9]


-- liftM all possible implementations that define a function
-- liftM :: Monad m => (a -> b) -> m a -> m b
-- that takes a function of type a -> b and "maps" it over a
-- non-bottom monadic value of type m a to produce a value of type m b
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = do a <- m
               return (f a)

liftM' f m = m >>= \a -> return (f a)

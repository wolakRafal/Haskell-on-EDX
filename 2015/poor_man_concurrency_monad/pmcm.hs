module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action = Atom (IO Action)
            | Fork Action Action
            | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- To express the connection between an expression of type Concurrent a and one of type Action,
-- we define a function `action :: Concurrent a -> Action` that transforms a `((a -> Action) -> Action)`
-- into an `Action` that uses `Stop :: Action` to create the continuation to the `Concurrent a`
-- passed as the first argument to action.
-- ===================================

action' :: ((a -> Action) -> Action) -> Action
action' fa = fa(\_ -> Stop)

action :: Concurrent a -> Action
action (Concurrent fa) = fa (\_ -> Stop)

-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent (\_ -> Stop)

-- ===================================
-- Ex. 2
-- ===================================

atom' :: IO a -> ((a -> Action) -> Action)
-- atom' ioA :: (IO a) = \c :: (a -> Action) -> ... :: Action
atom' ioA = \c -> Atom (ioA >>= \a -> return (c a))

atom :: IO a -> Concurrent a
--atom = error "aa"
atom ioA = Concurrent (\c -> Atom (ioA >>= \a -> return (c a)))


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork ca = Concurrent(\c -> Fork a1 (c ()))
        where
            a1 = action ca

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent fa1) = \(Concurrent fa2) -> Concurrent (\cont -> Fork (fa1 cont) (fa2 cont))


-- ===================================
-- Ex. 4
-- ===================================

-- helper function to understand implementation of '>>='
bind :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
bind f g =  lambda2 where
--                  lambda1 :: (b-> Action) -> a -> Action
                    lambda1 = \x -> \a -> (g a) x
--                  lambda2 :: (b -> Action) -> Action
                    lambda2 = \x -> f (lambda1 x)

instance Monad Concurrent where
    (Concurrent f) >>= g = (Concurrent lambda2)
                        where
                          lambda1 = \x -> \a -> case (g a) of
                                                  Concurrent fg -> fg x
                          lambda2 = \x -> f (lambda1 x)

    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = run stop    -- TODO check if this end condition is ok
roundRobin (x : xs) = case x of
                      Stop        -> roundRobin xs
                      Atom ioA    -> do a <- ioA              -- monadically execute computation,
                                        roundRobin (xs ++ [a])  -- put resulting process at the back of the process list.
                      Fork a1 a2  -> roundRobin ([a1, a2] ++ xs)    -- Fork creates two new processes


-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs


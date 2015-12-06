module IO_monad where

import Prelude hiding (getLine, putStr, putStrLn)

-- primitive getCh :: IO Char
--
-- getChar :: IO Char
-- getChar = do x <- getCh
--              putChar x
--              return x

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
             else
                do  xs <- getLine
                    return (x : xs)

putStr          :: String -> IO ()
putStr []       = return ()
putStr (x : xs) = do putChar x
                     putStr xs

putStrLn        :: String -> IO ()
putStrLn xs     = do putStr xs
                     putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr " The string has"
            putStr (show ( length xs))
            putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto        :: Pos -> IO ()
goto (x,y)  = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat     :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn        :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

putStr' xs = seqn [putChar x | x <- xs]

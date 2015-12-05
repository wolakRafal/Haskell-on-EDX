module Calculator where

import IO_monad
import Parsing

box :: [String]
box = [ "+---------------+",
        "|               |",
        "+---+---+---+---+",
        "| q | c | d | = |",
        "+---+---+---+---+",
        "| 1 | 2 | 3 | + |",
        "+---+---+---+---+",
        "| 4 | 5 | 6 | - |",
        "+---+---+---+---+",
        "| 7 | 8 | 9 | * |",
        "+---+---+---+---+",
        "| 0 | ( | ) | / |",
        "+---+---+---+---+"]

buttons     :: [ Char ]
buttons     = standard ++ extra
                  where
                    standard = "qcd=123+456-789*0()/"
                    extra    = "QCD \ESC\BS\DEL\n"

showbox     :: IO ()
showbox     = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box ]

display     :: String -> IO ()
display xs  = do writeat (3, 2) "             "
                 writeat (3, 2) (reverse (take 13 (reverse xs)))

calc        :: String -> IO ()
calc xs     =  do display xs
                  c <- getCh
                  if elem c buttons then
                    process c xs
                  else
                    do beep
                       calc xs

-- The function process takes a valid character and the current string,
-- and performs the appropriate action depending upon the character:
process                 :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs

-- We now consider each of the five possible actions in turn.
-- Quitting moves the cursor below the calculator box and terminates:
quit                    :: IO ()
quit                    = goto (1, 14)

delete                  :: String -> IO
delete " "              = calc " "
delete xs               = calc (init xs)

eval                    :: String -> IO ()
eval xs                 = case parse expr xs of
                            [(n, " ")] -> calc (show n)
                            _ -> do beep
                                    calc xs

clear                   :: IO ()
clear                   = calc " "

press                   :: Char -> String -> IO ()
press c xs              = calc (xs ++ [c])

run                     :: IO ()
run                     = do cls
                             showbox
                             clear
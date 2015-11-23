import Data.Char

type Bit = Int

bin2int		:: [Bit] -> Int
bin2int'	bits = sum [w*b | (w,b) <- zip weights bits]
			where weights = iterate (*2) 1

-- we conclude that bin2int can be rewritten using foldr :
bin2int	= foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
{-
We will ensure that all our binary numbers have the same length, eight
bits, by using a function make8 that truncates or extends a binary number as
appropriate to make it precisely eight bits -}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


-- Transmission
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- For example:
-- > encode "abc"
-- [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0]


{-To decode a list of bits produced using encode, we first define a function
chop8 that chops such a list up into eight-bit binary numbers -}

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode bits = map (chr . bin2int) (chop8 bits)
{-
Finally, we define a function transmit that simulates the transmission of
a string of characters as a list of bits, using a perfect communication channel
that we model using the identity function-}
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id 
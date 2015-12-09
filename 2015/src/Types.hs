module Types where
import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

-- Use Hugs for this lab (not ghci)

data Nat =  Zero
        |   Succ Nat
        deriving Show

------ natToInteger  --------
natToInteger :: Nat -> Integer

natToInteger (Succ n)   = natToInteger n +1
natToInteger Zero       = 0

natToInteger'   = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger''  :: Nat -> Integer
natToInteger''  = \ n -> genericLength [ c | c <- show n, c == 'S']

------ integerToNat  --------
integerToNat (n + 1) = Succ (integerToNat n)
integerToNat 0 = Zero

integerToNat' (n + 1) = let m = integerToNat' n in Succ m
integerToNat' 0 = Zero

-- all possible total and terminating implementations of an addition function
-- add :: Nat -> Nat -> Nat
-- that adds two non-bottom, non-partial, finite natural numbers m and n,
-- such that:
--              natToInteger (add m n) = natToInteger m + natToInteger n.
add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add m n)

add' n (Succ m) = Succ (add' m n)
add' n Zero = n

--- mult ----
-- Using recursion, function add,
-- implement a total and terminating multiplication function
-- mult :: Nat -> Nat -> Nat
-- that multiplies two non-bottom, non-partial, finite natural numbers m and n,
-- such that:
--              natToInteger (mult m n) = natToInteger m * natToInteger n.





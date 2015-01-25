import Data.List
import Data.Char
import Unsafe.Coerce


data Shape = Circle Float | Rect Float Float | Triangle

square			::	Float -> Shape
square n 		= Rect n n

area			::	Shape -> Float
area (Circle r)	= pi*r^2
area (Rect  x y)= x*y

-----------
data List a 	= 	Nil | Cons a (List a)
len				:: List a -> Int
len Nil			= 0
len (Cons _ xs) = 1 + len xs

-----

data Tree			= Leaf Int | Node Tree Int Tree
-- instance
t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))


occurs				:: Int -> Tree -> Bool
occurs	m (Leaf n)	= m==n
occurs m (Node l n r) = (n==m) || (occurs m l) || (occurs m r)

flatten				:: Tree -> [Int] 
flatten (Leaf x)	= [x]
flatten (Node l x r)  = flatten l ++ [x] ++ flatten r

---------------
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k, v) <- t , k == k]

---------------
data Prop	= 	Const Bool
			|	Var Char
			|	Not Prop
			|	And Prop Prop
			|	Imply Prop Prop

data Subst	=	Assoc Char Bool

--eval		:: Subst -> Prop -> Bool
--eval _ (Const b)	= b
--eval s (Var x)		= find x s
--eval s (Not p)		= not (eval s p)
--eval s (And p q)	= eval s p && eval s q
--eval s (Imply p q)	= eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply(Var 'A') (Var 'B'))) (Var 'B')

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

int2bin = unfold (==0) (`mod` 2) (`div` 2)

bools :: Int -> [[Bool ]]
bools 0 = [[ ]]
bools (n) = map (False:) bss ++ map (True:) bss
	where bss = bools (n - 1)

---------------------------------------------
--- Homework
---------------------------------------------

data Nat = Zero | Succ Nat deriving Show

integerToNat = head . m 
	where 	{
			; m 0 = [0]
			; m n = [sum [x | x <- (1:m (n - 1))]]
			} 


			
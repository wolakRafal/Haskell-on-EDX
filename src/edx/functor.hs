instance Functor [] where
	fmap = map

instance Functor Maybe where
	fmap f (Just x) = f (Just x)
	fmap f Nothing = Nothing
		
instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftSubTree rightSubTree) = Node (f x) (fmap f leftSubTree) (fmap f rightSubTree)   

-- class Either a b
-- :k Either = *->*->*
-- Functor f
-- :k Functor = (*->*) -> Constraint
instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x 


-- Homework:
-- how Map k is made an instance of Functor
instance Functor Map where
	func = 


--Kinds and some type-foo
class Tofu t where
	tofu :: j a -> t a j

{-
:k Tofu = ( *->(*->*)->* ) -> Constraint
j:: *->*
a:: *
t:: *->(*->*)->*

-}
-- let's make a type with a kind of * -> (* -> *) -> *.
data Frank a b  = Frank {frankField :: b a} deriving (Show)

{-
:k Frank = *->(*->*) -> * 
a:: *
b:: *->*
-}

instance Tofu Frank where
	tofu x = Frank x  

-- We have
data Barry t k p = Barry { yabba :: p, dabba :: t k }  
Barry :: (*->*) ->*->*-> *
-- Make this type a typeclass of Functor
class Functor (Barry a b) where
	fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
	 
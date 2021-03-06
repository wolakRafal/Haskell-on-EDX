------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
root (x :> xs) = x

children :: Rose a -> [Rose a]
children (x :> xs) = xs

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

-- this is the first first implementation, see if you cane improve this
size :: Rose a -> Int
size (x :> []) = 1
size (x :> xs) = 1 + (sum $ map size xs)

-- this is the first first implementation, see if you cane improve this
leaves :: Rose a -> Int
leaves (x :> []) = 1
leaves (x :> xs) = sum $ map leaves xs

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (x :> []) = (f x) :> []
  fmap f (x :> xs) = (f x) :> map (fmap f) xs

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a       = Sum a
newtype Product a   = Product a

instance Num a => Monoid (Sum a) where
  mempty        = Sum 0
  mappend s1 s2 = Sum (unSum s1 + unSum s2)
  
instance Num a => Monoid (Product a) where
  mempty        = Product 1
  mappend p1 p2 = Product (unProduct p1 * unProduct p2)

unSum :: Sum a -> a
unSum (Sum a) = a

unProduct :: Product a -> a
unProduct (Product a ) = a

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  -- It might be the case that we have a foldable data structure storing elements of type a that do not yet form a Monoid,
  -- but where we do have a function of type Monoid m => a -> m that transforms them into one.
  -- To this end it would be convenient to have a function foldMap :: Monoid m => (a -> m) -> f a -> m
  -- that first transforms all the elements of the foldable into a Monoid and then folds them into a single monoidal value.
  -- Add a default implementation of foldMap to the Foldable type class, expressed in terms of fold and fmap.
  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldMap f fa = fold (fmap f fa)

instance Monoid a => Monoid (Rose a) where
    mempty          = (mempty :> [])
    mappend (a1 :> children1) (a2 :> children2)  = (mappend a1 a2) :> (children1 ++ children2)

instance Foldable Rose where
    fold = foldr' (mappend) mempty where
                              foldr' f acc (a :> [])       = f a acc
                              foldr' f acc (a :> children) = foldr' f (f a acc) (foldr (mappend) mempty children)

  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a

-- cost - Constructor for Monoid value
-- unPackFun - unpack function, to extract value from Monoid
-- foldableButNotMonoid - foldable but not Monoid data structure
genericFun const unPackFun foldableButNotMonoid = unPackFun $ (foldMap (\a -> const a) foldableButNotMonoid)

fsum = genericFun (Sum) unSum
-- shorter version of:
-- fsum foldableButNotMonoid = genericFun (Sum) unSum foldableButNotMonoid

fproduct = genericFun (Product) unProduct
-- shorter version of:
-- fproduct foldableButNotMonoid = genericFun (Product) unProduct foldableButNotMonoid

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)


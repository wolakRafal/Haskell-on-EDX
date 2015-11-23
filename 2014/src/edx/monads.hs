data Expr = Val Int | Div Expr Expr

--data Maybe a = Nothing | Just a


sqr     :: [Int] -> [Int]
sqr []  = []
sqr (n:ns) = n^2: sqr ns

safediv			:: Int -> Int -> Maybe Int
safediv n m 	= if m == 0 then Nothing else Just (n `div` m)

eval            :: Expr -> Maybe Int
eval (Val n)    = Just n
eval (Div x y)  = case eval x of
					Nothing -> Nothing
					Just n	-> case eval y of
								Nothing -> Nothing
								Just m 	-> safediv n m


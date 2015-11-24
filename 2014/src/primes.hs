sieve :: [Int] -> [Int]
sieve (p:xs) = p : [x | x <- xs , x `mod` p /= 0]

primes :: [Int]

primes = sieve [2..]
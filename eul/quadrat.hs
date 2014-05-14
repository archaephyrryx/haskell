minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
    	| x < q     = x : sieve xs q ps
    	| otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t


isPrime :: Integer -> Bool
isPrime x | x <= 1 = False
          | otherwise = and [x`rem`p /= 0 | p<-(takeWhile (\y -> y^2 <= x) primes)]

quadrats :: Integer -> Integer -> Int
quadrats a b = length (takeWhile isPrime (map (\n -> n^2 + a*n + b) [0..]))

weave :: Integral a => [a] -> [a]
weave [] = []
weave (x:xs) | x == 0 = 0:weave xs
	     | otherwise = x:(-x):weave xs

solution :: Integer
solution = snd $ maximum [(quadrats a b, a*b)|b<-(takeWhile (<1000) primes),a<-(weave [1,3..999])]

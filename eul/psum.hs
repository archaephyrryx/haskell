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

psum :: Integer -> Integer
psum = \n -> sum $ takeWhile (<n) primes

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

defacto :: Integer -> Integer -> (Integer,Integer)
defacto x p | x == p = (1,1)
	    | x`mod`p==0 = (\(x,y) -> (x+1,y)) (defacto (x`div`p) p)
            | otherwise = (0,x)

prim :: (Integer,[Integer]) -> [(Integer,Integer)]
prim (x,(p:ps))  | p > x = []
                 | otherwise = (p,pow) : prim (redux,ps) 
			    where
				(pow,redux) = (defacto x p)


ndiv :: Integer -> Integer
ndiv = \x -> foldr (\(_,n) -> ((n+1)*)) 1 (prim (x,primes))

triang :: Integer -> Integer
triang n = (n*n + n)`div`2


maxim :: Ord a => [a] -> a
maxim (x:[]) = x
maxim (x:xs) = max x (maxim xs)

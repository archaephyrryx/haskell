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


pdiv :: Integer -> [(Integer,Integer)]
pdiv = \x -> prim (x,primes)

pow :: (Integer,Integer) -> [Integer]
pow (p,n) = map (p^) [0..n]

pows :: [(Integer,Integer)] -> [[Integer]]
pows = map pow

prod :: [Integer] -> [Integer] -> [Integer]
prod xs ys = [x*y|x<-xs,y<-ys]

dpows :: Integer -> [[Integer]]
dpows = pows.pdiv

divs :: Integer -> [Integer]
divs = \x -> (foldr (prod) [1] (dpows x)) `minus` (x:[])

divsum :: Integer -> Integer
divsum = sum.divs

amicable :: Integer -> Integer -> Bool
amicable a b = (a /= b) && (divsum a == b && divsum b == a)

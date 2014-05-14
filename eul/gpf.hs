defacto :: Integer -> Integer -> Integer
defacto x p | x == p = 1
	    | x`mod`p==0 = defacto (x`div`p) p
            | otherwise = x

gpf :: Integer -> Integer
gpf = \x -> prim (x,primes)

prim :: (Integer,[Integer]) -> Integer
prim (x,(p:ps)) | p > x = 1
		| (defacto x p) == 1 = p
		| otherwise = prim((defacto x p),ps)

n :: Integer
n = 600851475143

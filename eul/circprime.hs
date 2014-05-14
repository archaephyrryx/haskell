intFromList :: [Int] -> Int
intFromList = build.reverse
    where
	build :: [Int] -> Int
	build [] = 0
	build (x:xs) = x + 10*(build xs)

intToList :: Int -> [Int]
intToList = reverse.grind
    where
	grind :: Int -> [Int]
	grind 0 = []
	grind n = (n`rem`10):(grind (n`div`10))

cyclic :: Int -> Int
cyclic = intFromList.cyclist.intToList
	where
	    cyclist :: [a] -> [a]
	    cyclist (x:xs) = xs++[x]

cyclics :: Int -> [Int]
cyclics n = n:shifts
	where
	    shifts = takeWhile (/=n) cycles
	    cycles = tail (iterations)
	    iterations = iterate cyclic n


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

isPrime :: Int -> Bool
isPrime x = x == head [p | p<-primes, x`rem`p == 0]

isCircular :: Int -> Bool
isCircular = \x -> all isPrime (cyclics x)

solution :: [Int]
solution = [ p | p <- 2:3:5:(filter (\x -> not (any (\y -> even y || y`mod`5 == 0) (intToList x))) (takeWhile (<1000000) (drop 3 primes))), isCircular p ]

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

truncatel :: Int -> Int
truncatel = intFromList.tail.intToList

truncater:: Int -> Int
truncater = intFromList.init.intToList

truncations :: Int -> [Int]
truncations n = n:shifts
	where
	    shifts = takeWhile (/=0) weft
	    weft = weave (tail trunks) (tail trunks')
	    trunks = iterate truncatel n
	    trunks' = iterate truncater n

weave :: [a] -> [a] -> [a]
weave xs [] = xs
weave [] ys = ys
weave (x:xs) (y:ys) = x:y:weave xs ys


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
isPrime 0 = False
isPrime 1 = False
isPrime x = x == head [p | p<-primes, x`rem`p == 0]

isTrunk :: Int -> Bool
isTrunk = \x -> all isPrime (truncations x)

solution :: [Int]
solution = [ p | p <- filter good primes, isTrunk p ]
    where
	good :: Int -> Bool
	good 2 = False
	good 3 = False
	good 5 = False
	good 7 = False
	good x | any (==5) (tail xs) = False
	       | any even (tail xs) = False
	       | not $ isPrime (head xs) = False
	       | not $ isPrime (last xs) = False
	       | otherwise = True
	       where
		    xs = intToList x

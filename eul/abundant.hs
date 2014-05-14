minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs ys
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

divsum :: Integer -> Integer
divsum = \x -> product (map sum (pows (pdiv x)))

abundant :: Integer -> Bool
abundant x = (2*x < divsum x)

abundants :: [Integer]
abundants = [x|x<-[1..], abundant x]

pairwise :: [Integer] -> [Integer]
pairwise stream = (stride stream (repeat 0) (pair stream))

nubOrderedList :: (Ord k) => [k] -> [k]
nubOrderedList [] = []
nubOrderedList [k] = [k]
nubOrderedList (k1 : ks'@(k2 : _ks'')) | k1 < k2 = k1 : nubOrderedList ks'
				       | k1 == k2 = nubOrderedList ks'

alter :: (a->a) -> Int -> [a] -> [a]
alter = \f -> \n -> \xs -> (take (n) xs) ++ [f (xs !! n)] ++ (drop (n+1) xs)

inc :: Int -> [Int] -> [Int]
inc = alter (1+)

chop :: Int -> [[a]] -> [[a]]
chop = alter (tail)

defer :: [Int] -> [[a]] -> [(a,Int)]
defer cs xss = (infer (zip cs (zip (map head xss) [0..])))

infer :: [(Int,(a,Int))] -> [(a,Int)]
infer [] = []
infer ((c,xi):xis) | c == 0 = xi:[]
		   | otherwise = xi:(infer (dropWhile (\(p,(q,r)) -> p>=c) xis))

pair :: [Integer] -> [[Integer]]
pair = \xs -> map (\x->[x+y|y<-(dropWhile (<x) xs)]) xs

walk :: [Integer] -> [[Integer]] -> [Integer]
walk stream streams = stride stream counts streams
    where
	counts = (repeat 0)

stride :: [Integer] -> [Int] -> [[Integer]] -> [Integer]
stride xs cs xss = x : stride xs counts streams
    where
	(x,is) = step xs cs xss
	counts = foldr (\i -> inc i) cs is
	streams = foldr (\i -> chop i) xss is

step :: [Integer] -> [Int] -> [[Integer]] -> (Integer,[Int])
step xs cs xss = pace xs (defer cs xss)

pace :: [Integer] -> [(Integer,Int)] -> (Integer,[Int])
pace hs xs@((x,i):xt) = minim (x,(i:[])) hs xt
    where
        minim :: (Integer,[Int]) -> [Integer] -> [(Integer,Int)] -> (Integer,[Int])
        minim m _ [] = m
        minim m@(g,is@(i:_)) hs (y@(h,n):ynt) | g > h && 2*(hs !! n) > h = (h,[n])
                                              | g > h = minim (h,[n]) hs ynt
                                              | g == h && 2*(hs !! n) > h = (g,n:is)
                                              | g == h = minim (g,n:is) hs ynt
                                              | g < h && 2*(hs !! n) > g = m
                                              | g < h = minim m hs ynt

ways :: Integer -> Int
ways = \x -> sum [1|y<-(takeWhile (<=x`div`2) abundants),abundant (x-y)]

cantor :: [Integer]
cantor = [1..28124] `minus` (pairwise abundants)

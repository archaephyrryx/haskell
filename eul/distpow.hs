powerful :: [Integer] -> [Integer]
powerful stream = (stride stream (repeat 0) (pow stream))

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

pow :: [Integer] -> [[Integer]]
pow = \xs -> map (\x->[x^y|y<-xs]) xs

walk :: [Integer] -> [[Integer]] -> [Integer]
walk stream streams = stride stream counts streams
    where
	counts = (repeat 0)

stride :: [Integer] -> [Int] -> [[Integer]] -> [Integer]
stride xs cs xss | null xss = []
		 | null (head xss) = stride (tail xs) (tail cs) (tail xss)
		 | otherwise =  x : stride xs counts streams
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
        minim m@(g,is@(i:_)) hs (y@(h,n):ynt) | g > h && (hs !! n)^2 > h = (h,[n])
                                              | g > h = minim (h,[n]) hs ynt
                                              | g == h && (hs !! n)^2 > h = (g,n:is)
                                              | g == h = minim (g,n:is) hs ynt
                                              | g < h && (hs !! n)^2 > g = m
                                              | g < h = minim m hs ynt


uniq :: [Integer]
uniq = powerful [2..100]

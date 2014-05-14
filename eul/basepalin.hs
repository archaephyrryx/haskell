palin :: Int -> Bool
palin n = reverse (show n) == show n 

binPalin :: Int -> Bool
binPalin n = reverse m == m
    where
	m = decToBinList n

decToBinList :: Int -> [Int]
decToBinList = reverse.grind
    where
	grind :: Int -> [Int]
	grind 0 = []
	grind n = (n`rem`2):(grind (n`div`2))

solution :: [Int]
solution = [ x | x <- [1..1000000], palin x, binPalin x ]

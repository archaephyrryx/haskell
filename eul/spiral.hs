spiral :: Int -> [Int]
spiral 1 = [1]
spiral n = xs ++ (take 4 (tail (iterate (+(2*(n-1))) (last xs))))
    where
	xs = (spiral (n-1))

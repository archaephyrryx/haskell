qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort(x:xs) = qsort smaller ++ x:pivots ++ qsort larger
	      where
		    smaller = [a | a <- xs, a < x]
		    pivots = [b | b <- xs, b == x]
		    larger = [c | c <- xs, c > x]

digits :: Int -> [Int]
digits n = digitize (show n)
	where
	    digitize :: [Char] -> [Int]
	    digitize [] = [] 
	    digitize (x:xs) = (read (x:[]) :: Int):(digitize xs)

reduce :: (Int,Int) -> (Int,Int)
reduce (x,y) = uncommon (digits x, digits y)
    where
	uncommon ((x:x':[]),(y:y':[])) | x == y' && x' == y = (1,1)
	                               | x == y  && x' == y' = (1,1)
	                               | x == y = (x',y')
	                               | x == y' = (x',y)
	                               | x' == y' = (x,y)
	                               | x' == y = (x,y')
				       | otherwise = (10*x + x', 10*y + y')

solution = [(x,y) | y<-[11..99], x<-(takeWhile (<y) [11..99]), x`rem`10 /= 0, y`rem`10 /= 0, (x,y) /= reduce (x,y), y * (fst (reduce(x,y))) == x * (snd (reduce(x,y)))]

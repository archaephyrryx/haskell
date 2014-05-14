(!) :: Int -> Int
(!) n = product [1..n]


omit :: Int -> ([a] -> [a])
omit 0 = tail
omit n = (\(x:xs) -> x:(omit (n-1) xs))

permute :: Int -> Int -> ([a] -> [a])
permute 0 _ = id
permute n i | q == 0  = (\(x:xs) -> x:(permute (n-1) r xs))
	    | otherwise = (\xs -> (xs !! q):(permute (n-1) r (omit q xs)))
	    where
		 q = i `div` ((!) (n-1)) 
		 r = i `rem` ((!) (n-1)) 


lexperms :: Int -> [[Int]]
lexperms n = [permute n i ns | i <- [0..((!)n)-1]]
    where
	ns = [0..n-1]

isOrd :: Ord a => [a] -> Bool
isOrd xs = all (\(x,y) -> x <= y) (zip xs (tail xs))

showCat :: Show a => [a] -> String
showCat [] = ""
showCat (x:xs) = (show x)++(showCat xs)

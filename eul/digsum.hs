digsum :: Integer -> Integer
digsum n = sum (digitize (show n))
	where
	    digitize :: [Char] -> [Integer]
	    digitize [] = [] 
	    digitize (x:xs) = (read (x:[]) :: Integer):(digitize xs)

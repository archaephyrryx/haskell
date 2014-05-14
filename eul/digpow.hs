digPowSum :: Int -> Integer -> Integer
digPowSum k n = sum (map (^k) (digitize (show n)))
	where
	    digitize :: [Char] -> [Integer]
	    digitize [] = [] 
	    digitize (x:xs) = (read (x:[]) :: Integer):(digitize xs)

solution :: [Integer]
solution = [x | x <- [1..bound] , x == (digPowSum 5 x)]

bound :: Integer
bound = 5 * 9^5

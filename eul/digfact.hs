fact :: Int -> Int
fact n = product [1..n]

digFactSum :: Int -> Int
digFactSum n = sum (map fact (digitize (show n)))

digitize :: [Char] -> [Int]
digitize [] = [] 
digitize (x:xs) = (read (x:[]) :: Int):(digitize xs)

solution :: [Int]
solution = [x | x <-(filter viable [3..bound]), x == (digFactSum x)]

viable :: Int -> Bool
viable x | x > (fact) 9 = True
         | elem 9 xs = False
         | x > (fact) 8 = True
         | elem 8 xs = False
         | x > (fact) 7 = True
         | elem 7 xs = False
         | x > (fact) 6 = True
         | elem 6 xs = False
	 | x > (length xs)*(fact $ maximum xs) = False
	 | otherwise = True
    where
	xs = digitize $ show x

bound :: Int
bound = 7 * (fact 9)

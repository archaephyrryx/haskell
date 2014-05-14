digitFreq :: Int -> [Int]
digitFreq n = foldr (inc) zeros (digitize (show n))
	where
	    digitize :: [Char] -> [Int]
	    digitize [] = [] 
	    digitize (x:xs) = (read (x:[]) :: Int):(digitize xs)
	    zeros = replicate 10 0

notBad :: [Int] -> Bool
notBad xs = head xs == 0 && notOver (tail xs)

notOver :: [Int] -> Bool
notOver xs = all (<=1) xs

good :: Int -> Bool
good n = notBad xs && notMissing (tail xs)
    where
	xs = digitFreq n

notMissing :: [Int] -> Bool
notMissing xs = all (==1) (take d xs)
    where
	d = sum xs

necessary :: Int -> Int -> Bool
necessary x y = notBad zs
    where
	zs = (add (digitFreq x) (digitFreq y))

sufficient :: Int -> Int -> Bool
sufficient x y = notBad zs && sum (tail zs) == 9 && notMissing (tail zs) 
    where
	zs = (conseq (map digitFreq [x,y,x*y]))

conseq :: Num a => [[a]] -> [a]
conseq (xs:[]) = xs
conseq (xs:xss) = add xs (conseq xss)

alter :: (a->a) -> Int -> [a] -> [a]
alter = \f -> \n -> \xs -> (take (n) xs) ++ [f (xs !! n)] ++ (drop (n+1) xs)

inc :: Int -> [Int] -> [Int]
inc = alter (1+)

add :: Num a => [a] -> [a] -> [a]
add [] [] = []
add (x:xs) (y:ys) = (x+y):add xs ys 

pandigit :: Int -> Int -> Bool
pandigit x y = ( x`rem`10 > 1 &&
                 y`rem`10 > 1 &&
	 	 notBad (digitFreq x) &&
		 notBad (digitFreq y) &&
		 necessary x y &&
		 sufficient x y )

solution :: [Int]
solution = [x*y | y<-validy, x<-(takeWhile (\a -> a<y && a*y < 10^(9 - length (show a) - length (show y))) validx), pandigit x y]
    where
	validx = filter (notBad.digitFreq) [z|z<-[1..9999]]
	validy = filter (notBad.digitFreq) [z|z<-[1..9999]]



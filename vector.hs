dot :: Num a => [a] -> [a] -> a
dot [] [] = 0
dot xs ys = x*y + xss`dot`yss
	where
	    x = head xs
	    y = head ys
	    xss = tail xs
	    yss = tail ys

cross :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
cross = \(i,j,k) -> (\(l,m,n) -> (j*n - k*m, k*l - n*i, i*m - l*j))

sqnorm :: Num a => [a] -> a
sqnorm [] = 0
sqnorm (x:xs) = x*x + sqnorm xs

norm :: Num a => [a] -> Double
norm xs = sqrt((read (show (sqnorm xs)) :: Double) )

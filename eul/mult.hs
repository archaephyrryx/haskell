lcms :: [Integer] -> Integer
lcms [] = 1
lcms (1:xs) = lcms xs
lcms (x:xs) = lcm' x (lcms xs)

lcm' :: Integer -> Integer -> Integer
lcm' x y = (x*y)`div`(gcd' x y)

gcd' :: Integer -> Integer -> Integer
gcd' 0 x = x
gcd' x 0 = x
gcd' 1 _ = 1
gcd' _ 1 = 1
gcd' x y = gcd' (x`mod`y) (y`mod`x)

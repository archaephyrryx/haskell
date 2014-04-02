fact :: Integer -> Integer
fact n = product[1..n]

c :: Integer -> Integer -> Integer
a`c`b | b > a  = 0
      | b==0  = 1
      | b==a  = 1
      | otherwise = product([x|x<-[1..a],x>(a-b)]) `div` (fact b)



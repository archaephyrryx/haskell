uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:(uniq[y|y<-xs,y/=x])

factors :: Integer -> [Integer]
factors x = [n|n<-[1..x],x`mod`n==0]

isprime :: Integer -> Bool
isprime x = length(factors x) == 2

primes :: Integer -> [Integer]
primes x = [p|p<-[1..x],isprime p]

gc :: Integer -> Integer -> Integer
gc 1 _ = 1
gc _ 1 = 1
gc a b | a == 0   = b
        | b == 0   = a
	| otherwise  = gc (a`mod`b) (b`mod`a)

nroot :: Integer -> Integer -> Integer
nroot _ 0 = 0
nroot _ 1 = 1
nroot 1 x = x
nroot n x = (head [y|y<-[1..x],y^n>x]) - 1

hpow :: Integer -> Integer -> Integer
hpow p x | x`mod`p/=0  = 0
	 | otherwise = head [n|n<-[1..x],x`mod`(p^(n+1))/=0,x`mod`(p^n)==0]


pfact :: Integer -> [(Integer,Integer)]
pfact x = [(p,hpow p x)|p<-(primes x),hpow p x /= 0]


totient :: Integer->Integer
totient 1 = 1
totient x | isprime x = x-1
          | otherwise = product[(y-1)*y^(a-1)|(y,a)<-pfact x]


inv :: Integer -> Integer -> Integer
inv p x | gc p x == 1  = head [y|y<-[1..(p-1)],gc p y == 1,(x*y)`mod`p==1]
        | otherwise    = 0


ord :: Integer -> Integer -> Integer
ord p x | gc p x == 1  = head [y|y<-[1..(totient p)],(x^y)`mod`p==1]
        | otherwise    = 0 

gens :: Integer -> [Integer]
gens p | isprime p = [x|x<-[1..p-1],ord p x == p-1]
       | otherwise = []

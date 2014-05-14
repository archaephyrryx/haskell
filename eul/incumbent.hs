module Queue where
import Data.Maybe (fromMaybe)

data Queue k = E
             | T k [Queue k]
             deriving Show

fromOrderedList :: (Ord k) => [k] -> Queue k
fromOrderedList [] = E
fromOrderedList [k] = T k []
fromOrderedList (k1 : ks'@(k2 : _ks''))
  | k1 <= k2 = T k1 [fromOrderedList ks']

mergePairs :: (Ord k) => [Queue k] -> Queue k
mergePairs [] = E
mergePairs [q] = q
mergePairs (q1 : q2 : qs'') = merge (merge q1 q2) (mergePairs qs'')

merge :: (Ord k) => Queue k -> Queue k -> Queue k
merge (E) q2 = q2
merge q1 (E) = q1
merge q1@(T k1 q1's) q2@(T k2 q2's)
  = if k1 <= k2 then T k1 (q2 : q1's) else T k2 (q1 : q2's)

deleteMin :: (Ord k) => Queue k -> Maybe (k, Queue k)
deleteMin (E) = Nothing
deleteMin (T k q's) = Just (k, mergePairs q's)

toOrderedList :: (Ord k) => Queue k -> [k]
toOrderedList q
  = fromMaybe [] $
      do (k, q') <- deleteMin q
         return (k : toOrderedList q')

mergeOrderedByMin :: (Ord k) => [Queue k] -> Queue k
mergeOrderedByMin [] = E
mergeOrderedByMin (E : qs') = mergeOrderedByMin qs'
mergeOrderedByMin (T k q's : qs')
  = T k (mergeOrderedByMin qs' : q's)

nubOrderedList :: (Ord k) => [k] -> [k]
nubOrderedList [] = []
nubOrderedList [k] = [k]
nubOrderedList (k1 : ks'@(k2 : _ks''))
  | k1 < k2 = k1 : nubOrderedList ks'
  | k1 == k2 = nubOrderedList ks'


primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

defacto :: Integer -> Integer -> (Integer,Integer)
defacto x p | x == p = (1,1)
      | x`mod`p==0 = (\(x,y) -> (x+1,y)) (defacto (x`div`p) p)
            | otherwise = (0,x)

prim :: (Integer,[Integer]) -> [(Integer,Integer)]
prim (x,(p:ps))  | p > x = []
                 | otherwise = (p,pow) : prim (redux,ps) 
          where
        (pow,redux) = (defacto x p)
pdiv :: Integer -> [(Integer,Integer)]
pdiv = \x -> prim (x,primes)

pow :: (Integer,Integer) -> [Integer]
pow (p,n) = map (p^) [0..n]

pows :: [(Integer,Integer)] -> [[Integer]]
pows = map pow

divsum :: Integer -> Integer
divsum = \x -> product (map sum (pows (pdiv x)))

abundant :: Integer -> Bool
abundant 0 = False
abundant a = (2*a < (divsum a))

abundants :: [Integer]
abundants = [x|x<-[1..],abundant x]

minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs ys
	   GT ->     minus (x:xs)  ys
minus  xs     _     = xs

bountiful :: [Integer]
bountiful
  = nubOrderedList $ toOrderedList $
      mergeOrderedByMin
        [fromOrderedList (map (a +) (dropWhile (<a) abundants)) | a <- abundants]

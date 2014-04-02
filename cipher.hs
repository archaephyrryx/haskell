pairs :: String -> [(Char, Char)]
pairs s = zip s (tail s)

digraph :: String -> [(Char, Char)]
digraph s = [x|x<-p,nospace x]
    where
	p = pairs s

nospace :: (Char,Char) -> Bool
nospace (' ',_) = False
nospace (_,' ') = False
nospace _ = True

count :: Eq a => a -> [a] -> Int
count x xs = sum[1|y<-xs,y==x]

percent :: Int -> Int -> Float
percent = \n -> (\m -> (fromIntegral m / fromIntegral n)*100)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:(uniq[y|y<-xs,y/=x])

counts :: Eq a => [a] -> [(a,Int)]
counts xs = [(y,count y xs)|y<-uniq xs]

freqs :: Eq a => [a] -> [(a,Float)]
freqs xs = [(i, percent n c)|(i,c)<-cs]
    where
	n = sum[k|(_,k)<-cs]
	cs = counts(xs)

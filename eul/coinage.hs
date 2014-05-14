denoms :: [Int]
denoms = [1,2,5,10,20,50,100,200]

fill :: Int -> Int -> [Int] 
fill n x = takeWhile (<=(200)) (map (\z -> x + n*z) [0..])

fillUp :: Int -> [Int] -> [Int]
fillUp n = \xs -> concat $ map (fill n) xs

ways :: [Int]
ways = foldr (\x -> \ns -> fillUp x ns) [0] (tail denoms)

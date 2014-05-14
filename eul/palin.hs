palin :: Int -> Bool
palin n = reverse (show n) == show n

maxim :: Ord a => [a] -> a
maxim (x:[]) = x
maxim (x:xs) = max x (maxim xs)

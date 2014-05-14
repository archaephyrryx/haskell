collatz :: Int -> Int
collatz n | even n = n`div`2
          | odd  n = 3*n + 1

collate :: Int -> [Int]
collate = \x -> reverse $ qeq $ reverse [x]

qeq :: [Int] -> [Int]
qeq ns@(n:nt) | n == 1 = ns
	      | otherwise = qeq ((collatz n):ns)

trial :: [Int] -> (Int,Int) -> (Int,Int)
trial [] vm = vm
trial ns@(n:nt) vm = trial nt (max vm (extent,n))
		    where
			found = collate n
			extent = length found

tryharder :: Int -> (Int,Int)
tryharder = (map tryhard [0 ..] !!)
    where
	tryhard 0 = (1,1)
        tryhard (n+1) = trial [1000*(n)+1,1000*n+3..1000*(n+1)-1] (tryharder (n))

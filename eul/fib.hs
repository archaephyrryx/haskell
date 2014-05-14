fib' :: Int -> Integer
fib' = (map fib [0 ..] !!)
    where
	fib 0 = 0
	fib 1 = 1
        fib (n+1) = fib' n + fib' (n-1)

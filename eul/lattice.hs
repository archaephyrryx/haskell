(!) :: Integral int => int -> int
(!)n = product [1..n]

choose :: Integral int => int -> int -> int
a`choose`b = product [a-b+1..a] `div` product [1..b]

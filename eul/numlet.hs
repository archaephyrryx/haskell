wordy :: Int -> String
wordy 0 = ""
wordy 1 = "one"
wordy 2 = "two"
wordy 3 = "three"
wordy 4 = "four"
wordy 5 = "five"
wordy 6 = "six"
wordy 7 = "seven"
wordy 8 = "eight"
wordy 9 = "nine"
wordy x = ( (wordy (x`div`1000) ) `enjoin` "thousand")`conjoin`(join ((wordy((x`div`100)`mod`10))`enjoin`"hundred") "and" (word (x`mod`100)))
    where
	word :: Int -> String
	word  0 = ""
	word 10 = "ten"
	word 11 = "eleven"
	word 12 = "twelve"
	word 13 = "thirteen"
	word 14 = "fourteen"
	word 15 = "fifteen"
	word 16 = "sixteen"
	word 17 = "seventeen"
	word 18 = "eighteen"
	word 19 = "nineteen"
	word 20 = "twenty"
	word 30 = "thirty"
	word 40 = "forty"
	word 50 = "fifty"
	word 60 = "sixty"
	word 70 = "seventy"
	word 80 = "eighty"
	word 90 = "ninety"
	word x = (word (x-(x`mod`10)))++(wordy (x`mod`10))

join :: String -> String -> String -> String
join "" _ x  = x
join x _ "" = x
join x y z = x++y++z

conjoin :: String -> String -> String
x `conjoin` y = join x "" y

enjoin :: String -> String -> String
"" `enjoin` _ = ""
x `enjoin` y = x++y

adjoin :: String -> String -> String
_ `adjoin` "" = ""
x `adjoin` y = x++y

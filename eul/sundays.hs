data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Read, Show, Eq)

weekdays :: [Weekday]
weekdays = Monday:Tuesday:Wednesday:Thursday:Friday:Saturday:Sunday:weekdays

type Day = Int
type Year = Int

data Month = January
           | February
           | March
           | April
           | May
           | June
	   | July
	   | August
	   | September
	   | October
	   | November
	   | December
	deriving (Read, Show, Eq, Enum, Ord)


data Date = Date { jar :: Year,
		   mon :: Month,
	  	   tag :: Day }
	deriving (Read, Show)

instance Ord Date where
    compare d1 d2 = compare (jar d1, mon d1, tag d1) (jar d2, mon d2, tag d2)

instance Eq Date where
    (==) d1 d2 = (==) (jar d1, mon d1, tag d1) (jar d2, mon d2, tag d2)


datesOfYear :: Year -> [Date]
datesOfYear = \y -> concat [datesOfMonth y m | m <- [January .. December]]

datesOfMonth :: Year -> Month -> [Date]
datesOfMonth y m = [Date y m x | x <- [1..(final y m)]]
	where
		final _ m = case m of
						January -> 31
						February | y`mod`400 == 0 -> 29
								 | y`mod`100 == 0 -> 28
								 | y`mod`4 == 0 -> 29
								 | otherwise -> 28
						March -> 31
						April -> 30
						May -> 31
						June -> 30
						July -> 31
						August -> 31
						September -> 30
						October -> 31
						November -> 30
						December -> 31
						


alldates :: [(Date,Weekday)]
alldates = zip (concat (map datesOfYear [1900..2000])) weekdays

firstSundays :: Int
firstSundays = sum [1 | (x,y)<-alldates, x >= (Date 1901 January 1), x <= (Date 2000 December 31), tag x == 1, y == Sunday]

{-
1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible
by 4, but not on a century unless it is
divisible by 400.

How many Sundays fell on the first of the month
during the twentieth century (1 Jan 1901 to 31
Dec 2000)?
-}

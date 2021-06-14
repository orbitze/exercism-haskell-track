
{-- 				INSTRUCTIONS 
	
	Given a year, report if it is a leap year.

	The tricky thing here is that a leap year in the Gregorian calendar occurs:

	on every year that is evenly divisible by 4
	  except every year that is evenly divisible by 100
	    unless the year is also evenly divisible by 400

	For example, 1997 is not a leap year, but 1996 is. 1900 is not a leap year, but 2000 is.

	Hints
	To complete this exercise you need to implement the function isLeapYear that takes a year 
	and determines whether it is a leap year.
--}


module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = (rem year 4 == 0) && (rem year 100 /= 0 || rem year 400 == 0) 


{-  for my own understanding-- 

	2015	F && _		= F
	1970	F && _		= F
	1996	T && (T||_)	= T
	1960	T && (T||_) 	= T
	2100	T && (F||F)	= F
	1900	T && (F||F)	= F
	2000	T && (T||_)	= T
	2400	T && (T||_)	= T
	1800	T && (F||F)	= F
-} 

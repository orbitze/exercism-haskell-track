module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = (rem year 4 == 0) && (rem year 100 /= 0 || rem year 400 == 0) 


{-
	2015		F && ...			= F
	1970		F && ...			= F
	1996		T && (T||...)			= T
	1960		T && (T||...) 			= T
	2100		T && (F||F)			= F
	1900		T && (F||F)			= F
	2000		T && (T||...)			= T
	2400		T && (T||...)			= T
	1800		T && (F||F)			= F
-} 

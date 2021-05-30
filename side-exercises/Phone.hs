module Phone (number) where


number :: String -> Maybe String

number xs | isValid   = Just plainStr
          | otherwise = Nothing

           where plainStr = makePlain

                 makePlain | take 2 xs == "+1" = res 2
                           | take 1 xs == "1"  = res 1
                           | otherwise         = res 0

                            where res n = [x | x <- drop n xs, x >= '0', x <= '9']
                
                 isValid = length plainStr == 10
                           && not (elem (plainStr !! 0) "01")
                           && not (elem (plainStr !! 3) "01")






{- 
  "makePlain" function:
     takes the given string and returns another string consisting of only
     only such chars that represent numbers, disregards everything else.


  "isValid" predicate:
     checks whether the obtained 'plainStr' is of valid format: NXXNXXXXXX
     where N <- [2..9], X <- [0..9] and length of the phone no. should be 10.

     Note: The case where country code is any value other than '1', gets handled
           automatically.

           [If it is given and is of any value other than '1' or '+1', 
            the 'plainStr' comes out to be of length > 10, and so it 
            gets rejected inside the "isValid" predicate.]

-}




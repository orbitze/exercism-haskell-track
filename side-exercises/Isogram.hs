
{--                     INSTRUCTIONS 

      Determine if a word or phrase is an isogram.

      An isogram (also known as a "nonpattern word") is a word or phrase without a
      repeating letter, however spaces and hyphens are allowed to appear multiple times.

      Examples of isograms:
      -lumberjacks
      -background
      -downstream
      -six-year-old
      
      The word isograms, however, is not an isogram, because the s repeats.

--}


module Isogram (isIsogram) where
import Data.Char

isIsogram :: String -> Bool
isIsogram str = plainstr == (rmdups plainstr)
    where 
      plainstr = filter isAlpha (map toLower str)
      remdups = foldr (\x ls -> x : filter (/= x) ls) [] 


{-
    'plainStr' is the original string converted to lowercase 
     and rendered free of all punctuation marks

     <remdups> function removes duplicate letters from 'plainstr'  

     Next, comparison is made between the 2 so obtained strings
     If they match, the given string is an isogram, otherwise it's not.
-}


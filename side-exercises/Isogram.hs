
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
isIsogram str = plainStr == processedStr
    where 
      processedStr = foldl (\acc x -> if elem x acc then acc else acc ++ [x]) [] plainStr
      plainStr = [ch | ch <- map toLower str, ch `elem` ['a'..'z']]


{-
    'plainStr' is the original string, just converted to lowercase 
     and devoid of all the punctuation marks

    'processedStr' is the plainStr, just devoid of any repeated chars

     If plainStr and processedStr come to be equal strings, the input
     str is declared as an "isogram"  
-}
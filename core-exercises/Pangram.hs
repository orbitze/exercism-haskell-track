
{--                 INSTRUCTIONS

    Determine if a sentence is a pangram. 
    A pangram (Greek: παν γράμμα, pan gramma, "every letter") is a sentence using
    every letter of the alphabet at least once. The best known English pangram is:

    The quick brown fox jumps over the lazy dog.

    The alphabet used consists of ASCII letters a to z, inclusive, and is case 
    insensitive. Input will not contain non-ASCII symbols.

--}


module Pangram (isPangram) where
import Data.Char

isPangram :: String -> Bool
isPangram text = length processedStr == 26  
    where 
      processedStr = foldl (\acc x -> if isValid x then (if elem x acc then acc else acc++[x]) else acc) [] (map toLower text)
      isValid x = x `elem` ['a'..'z']


{- Explanation of the approach:
   
   Step1. Input string is converted to lowercase, and is then sent to the
         'processedStr' function which returns a string containing only the 
          non-repeated characters from the input string, ignoring all 
          non-alphabetic chars.

   Step2. Length test: If the length of the string obtained in Step1 is equal
          to 26, input string is a PANGRAM otherwise it is not. 

-}


module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification

classify n | n < 1           = Nothing
           | aliquotSum == n = Just Perfect 
           | aliquotSum > n  = Just Abundant 
           | otherwise       = Just Deficient

           where aliquotSum = sum [x | x <- [1..(div n 2)], rem n x == 0]


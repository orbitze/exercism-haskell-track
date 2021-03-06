
{-                                INSTRUCTIONS

	Implement the keep and discard operation on collections. 
	
	Given a collection and a predicate on the collection's elements, keep returns a new collection 
	containing those elements where the predicate is true, while discard returns a new collection 
	containing those elements where the predicate is false.

	For example, given the collection of numbers:

	1, 2, 3, 4, 5
	And the predicate:
	is the number even?
	
	Then your keep operation should produce:
	2, 4
	While your discard operation should produce:
	1, 3, 5
	
	Note that the union of keep and discard is all the elements.

	Restrictions
	Keep your hands off that filter/reject/whatchamacallit functionality provided by your standard library! 
	Solve this one yourself using other basic tools instead.

-}

module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x] 

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = keep (not . p) xs


{-	RECURSIVE SOLUTION:

    keep :: (a -> Bool) -> [a] -> [a]
    keep _ [] = []
    keep p (x:xs) | p x = x : keep p xs
		  | otherwise = keep p xs 

    discard :: (a -> Bool) -> [a] -> [a]
    discard p xs = keep (not . p) xs
-}

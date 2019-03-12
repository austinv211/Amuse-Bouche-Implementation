module Part4 where

-- A will be a type that can do equality
-- Given a list of a, return a list of (a, Int)
-- This function compresses a string to tuples.
-- The function groups each character into a tuple of the character, and the number of consecutive occurrences
-- i.e "aabccca" -> [(a, 2), (b, 1), (c, 3), (a, 1)]
runLengthEncode :: Eq a => [a] -> [(a, Int)]
-- If input list is empty, return empty list
runLengthEncode [] = []
runLengthEncode (x:xs) = nextGroup x 1 xs
  where
  -- Base case fpr next nextGroup
  -- When we reache the end of the list (as shown by empty list in third parameter),
  --    we will store the last character found (e) and the number of consecutive occurrences tracked (n)
    nextGroup e n [] = [(e, n)]
  -- If the third parameter (list) has at least one element
    nextGroup e n (y:ys)
  -- If e == y, we will can increment n by 1 to denote that we found another consecutive e,
  -- this will allow us to store the same (consecutive) character in one tuple
      | e == y    =          nextGroup e (n + 1) ys
  -- When we find a different character than the last one, we will create a tuple for the last character's streak,
  --    and start tracking the next tuple.
      | otherwise = (e, n) : nextGroup y  1      ys

-- This function validates that the total number of Integer represented in a runLengthEncode output
--    is equal to the initial input list's length
-- This function should work for any Eq a => [a] list as well (if we did not specify [Int])
rlePropLengthPreserved :: [Int] -> Bool
-- given the input as, the length of the as will be equal to the sum of the second values of the runLengthEncode output
rlePropLengthPreserved as = length as == (sum $ map snd $ runLengthEncode as)

-- This function tests our runLengthEncode function for strings of the same character with up to length 99
rlePropDupesCollapsed :: Int -> Bool
rlePropDupesCollapsed n
  -- if m is 0, 0 characters is the empty string. This should return [] which is what is tested.
  -- This case is necessary because although replicate 0 _ returns "", the output will be the empty list and not ['x', 0]
  | m == 0    = runLengthEncode "" == []
  -- if m > 0
  --   we will run our runLengthEncode function on m xs.
  --   replicate creates a list of the inputted character or string m amount of times
  --   because a string is a list of characters, replicate on a character becomes a string
  | otherwise = runLengthEncode (replicate m 'x') == [('x', m)]
  -- based on the input, we choose a reasonable (< 100) number to replicate by
  where m = n `mod` 100

rlePropRoundTrip :: [Int] -> Bool
rlePropRoundTrip ns = runLengthEncode xs == is
  -- The Map will create convert ns into reasonable numbers (in range from 1..100)
  -- After mapping, we can zip each number to an ascii character starting from 'a'
  -- i.e. [('a',1),('b',2),('c',3),('d',4),('e',5)]
  where is = zip ['a'..] $ map (\n -> n `mod` 100 + 1) n
  -- xs will convert the zip into a string (like a reverse runLengthEncode) and concatenate each result to each other
        xs = concatMap (\(i,n) -> replicate n i) is

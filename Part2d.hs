module Part2d where

-- An Example of using Maybe
pickMessage :: Maybe Int -> String
-- If the Maybe is a number:
pickMessage (Just n) = "Pick a number, like " ++ show n ++ "."
-- Elif the Maybe is Nothing
pickMessage Nothing = "Pick any number you like."

-- Finds the first character after a star
-- This returns maybe because there might be no stars or characters after star
findAfterStar :: String -> Maybe Char
-- c is the current (first) character : d is the second character : r is the rest of the list (could be [])
findAfterStar (c:d:r) =
-- If the current character is a *
--    we can return the second character. 
--    Note: The pattern matching gurantees d exists
-- If the current character is not *, continue on the rest of the list
  if c == '*' then Just d
              else findAfterStar (d:r)
			  
-- For anything that doesnt satisfy (c:d:r) a.k.a any string of length 0 or 1.
findAfterStar _ = Nothing


-- Similar to findAfterStar but you can enter the character to search for instead of *
findAfterChar :: Char -> String -> Maybe Char
-- Same patern matching as findAfterStar
findAfterChar m (c:d:r) =
  if c == m then Just d
  -- Note, recursive call still needs to input m
            else findAfterChar m (d:r)
findAfterChar _ _ = Nothing

-- More generic version of findAfterElem
-- Eq a => a states that a's can be checked for equality
-- This still works on strings since strings are character arrays
findAfterElem :: Eq a => a -> [a] -> Maybe a
-- Exact same logic as findAfterChar
findAfterElem m (c:d:r) =
  if c == m then Just d
            else findAfterElem m (d:r)
findAfterElem _ _ = Nothing



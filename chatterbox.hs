
--The function substitute wildcard t s replaces each occurrence of the element wildcard (ch) in the list xs with the list ys

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute ch (x:xs) ys 
    |ch == x    = ys ++ substitute ch xs ys
    |otherwise  = x : substitute ch xs ys 

--The function match wildcard p s tries to match the two lists p and s. The list p is considered a pattern which may contain elements equal to wildcard. The list s may not contain any wildcards. The pattern p is said to match the list s if every element in p matches corresponding elements in s. A non-wildcard matches a single element in s if they are equal and a wildcard matches an arbitrarily long (non-empty) sublist. If the two lists match, the function returns Just r where r is the sublist in s which matches the first occurence of wildcard in p. If the lists don't match, the result is Nothing.

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Nothing
match _ _ [] = Nothing
match _ [] _ = Nothing
--match wildcard pattern list
--    |

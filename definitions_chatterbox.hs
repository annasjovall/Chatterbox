
--map over a tuple, function 1 maps over x1 (kan vara lista). 
--ex:  map2 ((++ "!"), (++ "A")) ("anna","panna")
-- = ("anna!","pannaA")
--ex2: map2 ((head), (head)) ([1,2,3],[3,4,5])
-- = (1,3)
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

--Som en map fast fungerar för optionals
--ex: mmap head Nothing
-- = Nothing
--ex2: mmap head (Just [1,2,3])
-- = Just 1

mmap :: (a -> b) -> Maybe a -> Maybe b --mabye = optional
mmap f  Nothing  = Nothing --nothing = tom optiona
mmap f (Just x)  = Just (f x) --just = optional med innehåll


--orElse (Nothing) (Just [1,2,3]) 
--ta första värdet först, annars ta det andra
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

--Try and apply the function to x
--den retunerar x om (f x) blir Nothing,annars retuneras id (f x) = f x
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

--The Eq class defines equality (==) and inequality (/=)ra
--a måste ha en eq i sig (implementera equals)
--Körs tills den hittar en idiompotent funktion
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

--The RealFrac subclass of Fractional and Real provides a function properFraction, which decomposes a number into its whole and fractional parts, and a collection of functions that round to integral values by differing rules: 
--floor : returns the greatest integer not greater than argument (skär av decimaler)
-- !! :  It takes a list and an index, and returns the item at that index. 
--[1,2,3,4] !! 3 = 4
-- . function composition, start from right
-- integral - no fractions
-- längden blir en integral, muliplies with r, removes decimals = index ger ett element
--frac mellan 0 och 1, ger den procenten av listan
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xsk

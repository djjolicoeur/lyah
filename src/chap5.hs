-- Exercises from chap5, high order fns and fn currying

-- basic fn that compares and in w/ x

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

-- We can rewrite the same fn with a partial fn on compare.  These fns are equiv.

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- sections may be used to create partials of infix fns via parens

divideByTen :: (Floating a) => a -> a
divideByTen = ( / 10)

-- More partials

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- High order fns

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- zipWith' apply f to each x,y pair in x:xs and y:ys 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- change order of args

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

-- Easier flip.  note the change in type signature


flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

-- EX: zipWith' (flip'' div) [2,2..] [2,4,6,8,10,12]

-- map' map clone.  is mapcat map++ in haskell?

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter' , filter clone

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs


quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerOrEqual = filter' (<= x) xs
      larger         = filter' (> x) xs
  in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger

-- Find largest number < 100000 divisible by 3829

largestDivisable :: Integer
largestDivisable = head (filter p [100000,99999..])
  where p x = mod x 3829 == 0

-- Find sum of all odd squares < 10000

sumExample :: Integer
sumExample = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Sum of all odd squares <1000 but with list comprehensions

sumExample' :: Integer
sumExample' = sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])

-- Fun with the collatz conjecture

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (div n 2)
  | odd n = n:chain (n*3 + 1)

-- how many collatz sequences, starting between 1 and 1000 have chains > 15?

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..1000]))
  where isLong xs = length xs > 15

-- lambdas! same fn as above but with lambdas

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..1000]))

-- Interesting example illustrating how fns are curried by default.

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- is equivalent to, albeiet more readable

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

-- FLip with a lambda (even more readable)

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

-- Folds

-- sum with left fold

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- a sum thats even more simple

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0


-- map with right fold

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- elem with right fold

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldl1 & foldr1

-- Maximum with foldl1

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- reverse w/ foldl

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- Reverse but with flip (:), which is the equivalent of (\acc x -> x : acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- product with foldl

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- Filter via foldr

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if (p x) then x : acc else acc) []

-- Last with foldl1

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- and with foldr, and' (repeat False) terminates, and' (repeat True) because
-- of pattern matching in && immediately evals to False if the first param is
-- False

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- scanl and scanr, like foldl,foldr but includes all intermediate acc states

-- How many elements does it take for the sum of the square roots of all
-- natural numbers to exceed 1,000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $ (right assosociation), . (function compositions), and point-free

-- without 

oddSquareSums :: Integer
oddSquareSums = sum (takeWhile (<1000) (filter odd (map (^2) [1..])))

-- equivalent

oddSquareSums' :: Integer
oddSquareSums' = sum . takeWhile (<1000) . filter odd $ map (^2) [1..]

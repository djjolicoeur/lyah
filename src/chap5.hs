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

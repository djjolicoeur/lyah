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



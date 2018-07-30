-- Examples, etc from LYAH chap 2.  Types

-- Factorial with Integer
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Int is Bound, the above impl with Integer is not. 

overflowingFactorial :: Int -> Int
overflowingFactorial n = product [1..n]

-- Float,  Single precision floating point numbers

circumference :: Float -> Float
circumference r = 2 * pi * r

-- Double, Double precision floating point numbers

circumference' :: Double -> Double
circumference' r = 2 * pi * r



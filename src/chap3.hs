-- LYAH chap 3, Functions defs w/ Pattern Matching...I get it now.

-- Not Great version of addVectors w/ out pattern matching

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' a b = (fst a + fst b, snd a + snd b)

-- Better impl using pattern matching. think clojure (let [{a :a b :b}]))

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- definitions on triples w/ pattern matching

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,x,_) = x

third :: (a,b,c) -> c
third (_,_,x) = x

-- Pattern matching in list Comprehensions w/ tuples

sumTup :: [(Integer, Integer)] -> [Integer]
sumTup xs = [a+b | (a,b) <- xs]

-- on lists, lets make our own version of head, head'

head' :: [a] -> a
head' [] = error "Can't call head on empty list!!"
head' (x:_) = x

-- impractical, but illustrative eaxmple of pattern matching args

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long.  The first two elements are " ++ show x
  ++ " and " ++ show y

-- as-pattern, e.g. clojure (let [{a :a b :b :as all }])

firstLetter :: String -> String
firstLetter "" = "Empty String, Whoops!!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards, Guards!!

bmiTell :: Double -> String -- Note no equals
bmiTell bmi                 -- none here, either
  | bmi <= 18.5 = "You're underweight, you emo you"
  | bmi <= 25.0 = "You're supposedly normal. pfft. I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

-- Multiple args

bmiHeightTell :: Double -> Double -> String
bmiHeightTell weight height
  | weight / height ^ 2 <= 18.5 =  "You're underweight, you emo you"
  | weight / height ^ 2 <= 25.0 =  "You're supposedly normal. pfft. I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"


-- Some comarison functions

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

-- we can even define a fn with infix backticks

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

-- We can use a where clause to avoid repeated calculations as in bmiHeightTell

bmiHeightTell' :: Double -> Double -> String
bmiHeightTell' weight height
  | bmi <= 18.5 =  "You're underweight, you emo you"
  | bmi <= 25.0 =  "You're supposedly normal. pfft. I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

-- Multiple Where clauses

bmiHeightTell'' :: Double -> Double -> String
bmiHeightTell'' weight height
  | bmi <= skinny =  "You're underweight, you emo you"
  | bmi <= normal =  "You're supposedly normal. pfft. I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0


-- Scope: where clause no shared across pattern matched fn bodies

badGreeting :: String
badGreeting = "Oh, pfft. It's you. "

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you, "

greet :: String -> String
greet "Juan" = niceGreeting ++ "Juan!"
greet "Fernando" = niceGreeting ++ "Fernando!"
greet name = badGreeting ++ name ++ "."

-- Pattern matching in the where, even better BMI

bmiHeightTell''' :: Double -> Double -> String
bmiHeightTell''' weight height
  | bmi <= skinny =  "You're underweight, you emo you"
  | bmi <= normal =  "You're supposedly normal. pfft. I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5,25.0,30.0)


initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- Fns defined in where

calcBmis :: [(Double,Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

-- let bindings.  Note the in syntax.

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- Let in list comprehensions

calcBmis' :: [(Double,Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

-- Case syntax

describeList :: [a] -> String
describeList ls =
  "The list is " ++
  case ls of [] -> "empty."
             [a] -> "a singleton list."
             xs -> "a longer list."

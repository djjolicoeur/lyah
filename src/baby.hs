-- Learn you a Haskell chap 1, baby.hs 

-- Functions

doubleMe x = x + x
 
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1
                           
conanO'Brien = "It's a-me, Conan O'Brien!"

lol = "LOL "

lulz x = take ((x * length lol) - 1) (cycle lol)

-- List Comprehensions

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

-- Tuples

rightTriangles = [ (a,b,c)
                 |c <- [1..10],
                  a <- [1..c],
                  b <- [1..a],
                  a^2 + b^2 == c^2,
                  a + b + c == 24]                        

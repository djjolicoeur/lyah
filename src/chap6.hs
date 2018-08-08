-- LYAH Chap 6. Modules.

-- import modules w/ import. In GHCi (repl), use :m Data.List

-- You can selevtively import fns as well
-- Import Data.List (nub, sort)
-- Conversly you can hide fns
-- Import Data.List hiding (nub) if we have ourown nub fn

-- To avoid clashes we can do qaulifeid imports vis `as` like below
-- just like clojure (:require [clojure.string :as str])

import Data.List as L
import Data.Char as C

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . L.group . sort . L.words

-- isIn: is needle a sublist of haystack?

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = L.any (needle `L.isPrefixOf`) (L.tails haystack)


-- Caeser cipher

encode :: Int -> String -> String
encode offset msg = map (\c -> C.chr $ C.ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg






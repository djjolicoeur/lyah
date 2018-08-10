-- LYAH Chap 6. Modules.

-- import modules w/ import. In GHCi (repl), use :m Data.List

-- You can selevtively import fns as well
-- Import Data.List (nub, sort)
-- Conversly you can hide fns
-- Import Data.List hiding (nub) if we have ourown nub fn

-- To avoid clashes we can do qaulifeid imports vis `as` like below
-- just like clojure (:require [clojure.string :as str])

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub

wordNums :: String -> [(String, Int)]
wordNums =
  map (\ws -> (head ws, length ws)) . List.group . List.sort . List.words

-- isIn: is needle a sublist of haystack?

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = List.any (needle `List.isPrefixOf`) (List.tails haystack)


-- Caeser cipher

encode :: Int -> String -> String
encode offset msg = map (\c -> Char.chr $ Char.ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- Sum digits in a number

digitSum :: Int -> Int
digitSum = sum . map Char.digitToInt . show

-- First to 40

firstTo40 :: Maybe Int
firstTo40 = List.find (\x -> digitSum x == 40) [1..]

-- First to n

firstTo :: Int -> Maybe Int
firstTo n = List.find (\x -> digitSum x == n) [1..]

-- Assiciation Lists, Dictionaries...build our own maps..?

phoneBook =
  [(" betty", "555-2938")
  ,(" bonnie", "452-2928")
  ,(" patsy", "493-2928")
  ,(" lucille", "205-2928")
  ,(" wendy", "939-8282")
  ,(" penny", "853-2492")]

-- get

findKey :: (Eq k) => k  -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey key xs

-- via foldr

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key xs =
  foldr
  (\(k,v) acc ->
     if key == k
     then Just v
     else acc)
  Nothing
  xs

-- Maps!

phoneBook' :: Map.Map String String
phoneBook' = Map.fromList $
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")]

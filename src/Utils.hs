module Utils where 

import Data.Binary
import Data.Char
import Data.List.Split

import HuffmanTree

-- BINARY & DECIMAL --
-- decToBin . binToDec == id

binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0 . reverse

decToBin 0 = []
decToBin n | n `mod` 2 == 1 = decToBin (n `div` 2) ++ [True]
           | n `mod` 2 == 0 = decToBin (n `div` 2) ++ [False]

fillBinary :: [Bool] -> [Bool]
fillBinary x = take (7 - length x) (repeat False) ++ x

-- ADD APPENDIX --
-- addAppendix :: [Bool] ->
-- default byte size = 7
-- addAppendix . removeAppendix = id

addAppendix :: [Bool] -> [Bool]
addAppendix x = x++(take (7 - mod (length x) 7) (repeat False)) ++ (fillBinary $ decToBin $ 7 - mod (length x) 7)

getAppendixSize :: [Bool] -> Int
getAppendixSize = (+7) . binToDec . reverse . take 7 . reverse

removeAppendix :: [Bool] -> [Bool]
removeAppendix x = take (length x - getAppendixSize x) x

-- Make list of chars
-- binToCharList . charListToBin = id

binToCharList :: [Bool] -> String
binToCharList x = map chr $ map binToDec $ chunksOf 7 x

charListToBin :: String -> [Bool]
charListToBin x = concat $ map (fillBinary . decToBin) $ map ord x

-- Write/Load Tree on disk

saveTree :: String -> HuffmanTree -> IO ()
saveTree path tree = encodeFile path tree

loadTree :: String -> IO HuffmanTree
loadTree path = decodeFile path >>= \x -> return x

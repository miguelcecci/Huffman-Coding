module Utils where 

import Data.Binary
import Data.Char
import Data.List.Split

import HuffmanTree

defaultByteSize :: Int
defaultByteSize = 7

-- BINARY & DECIMAL --
-- decToBin . binToDec == id

binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0 . reverse

decToBin 0 = []
decToBin n | n `mod` 2 == 1 = decToBin (n `div` 2) ++ [True]
           | n `mod` 2 == 0 = decToBin (n `div` 2) ++ [False]

fillBinary :: [Bool] -> [Bool]
fillBinary x = take (defaultByteSize - length x) (repeat False) ++ x

-- ADD APPENDIX --
-- default byte size = defaultByteSize
-- addAppendix . removeAppendix = id

addAppendix :: [Bool] -> [Bool]
addAppendix x = x++(take (defaultByteSize - mod (length x) defaultByteSize) (repeat False)) ++ (fillBinary $ decToBin $ defaultByteSize - mod (length x) defaultByteSize)

getAppendixSize :: [Bool] -> Int
getAppendixSize = (+defaultByteSize) . binToDec . reverse . take defaultByteSize . reverse

removeAppendix :: [Bool] -> [Bool]
removeAppendix x = take (length x - getAppendixSize x) x

-- Make list of chars
-- binToCharList . charListToBin = id

binToCharList :: [Bool] -> String
binToCharList x = map chr $ map binToDec $ chunksOf defaultByteSize x

charListToBin :: String -> [Bool]
charListToBin x = concat $ map (fillBinary . decToBin) $ map ord x

-- Write/Load Tree on disk

saveTree :: String -> HuffmanTree -> IO ()
saveTree path tree = encodeFile path tree

loadTree :: String -> IO HuffmanTree
loadTree path = decodeFile path >>= \x -> return x

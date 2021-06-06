import Data.List
import Data.Char
import Data.Binary
import Data.List.Split

import HuffmanTree

main :: IO ()
main = do
  print "teste" 
-- examples 

exp_text :: String
exp_text = "Integer euismod ac turpis ut feugiat. Duis luctus odio turpis, a ornare metus fermentum sit amet. Vivamus viverra hendrerit dictum. Vivamus porta, leo et mattis pharetra, augue nunc finibus velit, id sollicitudin orci odio ultrices augue. Nam libero ligula, convallis sed suscipit nec, fermentum sed nunc. Etiam justo eros, rutrum sit amet libero eget, euismod rutrum dui. Duis ut nisi non orci finibus faucibus. Aliquam risus lorem, bibendum et nulla ut, maximus blandit nibh. Nam eget aliquam metus. Vestibulum vel ante non tellus faucibus suscipit. Donec nec nulla in urna aliquet mattis in ac mauris. Donec euismod, diam in accumsan blandit, ipsum purus aliquet risus, ut dictum velit sapien luctus enim. Vestibulum eros dui, feugiat sit amet iaculis in, aliquet et justo. Morbi pellentesque bibendum consequat. Aliquam iaculis gravida neque et interdum."

exp_tree :: HuffmanTree
exp_tree = build_tree exp_text 


-- Uses a string as input and return list of (number of ocurrences, the char)
char_count :: String -> [(Int, Char)]
char_count my_string = sort $ zip counts chars
    where chars = map (!! 0) $ group $ sort my_string
          counts = map length $ group $ sort my_string

-- Transforms a pair of (Int, char) in Leaf (Int, [Char])
make_leaf :: (Int, Char) -> HuffmanTree
make_leaf (a, b) = Leaf (a, [b])

-- Returns the weight of a HuffmanTree
weight :: HuffmanTree -> Int
weight (Leaf a) = fst a 
weight (Node _ a _) = fst a

-- Returns the list of chars stored in the tree
chars :: HuffmanTree -> [Char]
chars (Leaf a) = snd a 
chars (Node _ a _) = snd a

-- returns the merge of a huffman tree
merge :: HuffmanTree -> HuffmanTree -> HuffmanTree 
merge a b = Node a (weight a + weight b, chars a ++ chars b) b

-- merge a list of leafs on a single tree
merge_until :: [HuffmanTree] -> HuffmanTree
merge_until [x] = x
merge_until (a : b : xs) = merge_until $ sort $ merge a b : xs

-- build huffman tree from text
build_tree :: String -> HuffmanTree 
build_tree = merge_until . map make_leaf . char_count

-- ENCODE TEXT --

-- encode a single char
-- TODO: discover best way to trow exeption when char not in tree
encode_char :: Char -> HuffmanTree -> [Bool] -> [Bool]
encode_char a (Node left center right) acc =  if not $ elem a $ snd center
    then [False] --jogar exception aqui 
    else if elem a $ chars left 
        then encode_char a left (acc++[False])
        else encode_char a right (acc++[True])  
encode_char a (Leaf _) acc = acc

encode_text :: String -> HuffmanTree -> [Bool]
encode_text (x:xs) htree = (encode_char x htree []) ++ (encode_text xs htree)
encode_text _ _ = []

-- DECODE TEXT --
decode_char :: [Bool] -> HuffmanTree -> (Char, [Bool])
decode_char (x:xs) (Node left center right) = if x
    then decode_char xs right
    else decode_char xs left
decode_char x (Leaf (_, a)) = (a !! 0, x)

decode_text :: [Bool] -> HuffmanTree -> String
decode_text [] htree = ""
decode_text x htree = [a] ++ decode_text b htree
    where (a, b) = decode_char x htree

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

binToCharList :: [Bool] -> [Char]
binToCharList x = map chr $ map binToDec $ chunksOf 7 x

charListToBin :: [Char] -> [Bool]
charListToBin x = concat $ map (fillBinary . decToBin) $ map ord x



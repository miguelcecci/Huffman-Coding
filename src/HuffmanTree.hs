module HuffmanTree where

import Control.Monad
import Data.Binary
import Data.List

data HuffmanTree = Leaf (Int, String)
                   | Node (HuffmanTree) (Int, String) (HuffmanTree)
  deriving (Eq, Ord, Show)

instance Binary HuffmanTree where
  put (Leaf (a, b)) = do put (0 :: Word8)
                         put a
                         put b
  put (Node t1 (s1, s2) t2) = do put (1 :: Word8)
                                 put t1
                                 put s1
                                 put s2
                                 put t2

  get = do t <- get :: Get Word8
           case t of
                0 -> do a <- get
                        return (Leaf a)
                1 -> do t1 <- get
                        s1 <- get
                        s2 <- get
                        t2 <- get
                        return (Node t1 (s1, s2) t2)

  -- get = do tag <- getWord8
  --          case tag of
  --               0 -> liftM Leaf get
  --               1 -> liftM3 Node get get get


-- Tree functions

-- Uses a string as input and return list of (number of ocurrences, the char)
charCount :: String -> [(Int, Char)]
charCount my_string = sort $ zip counts chars
    where chars = map (!! 0) $ group $ sort my_string
          counts = map length $ group $ sort my_string

-- Transforms a pair of (Int, char) in Leaf (Int, String)
makeLeaf :: (Int, Char) -> HuffmanTree
makeLeaf (a, b) = Leaf (a, [b])

-- Returns the weight of a HuffmanTree
weight :: HuffmanTree -> Int
weight (Leaf a) = fst a 
weight (Node _ a _) = fst a

-- Returns the list of chars stored in the tree
chars :: HuffmanTree -> String
chars (Leaf a) = snd a 
chars (Node _ a _) = snd a

-- returns the merge of a huffman tree
merge :: HuffmanTree -> HuffmanTree -> HuffmanTree 
merge a b = Node a (weight a + weight b, chars a ++ chars b) b

-- merge a list of leafs on a single tree
mergeUntil :: [HuffmanTree] -> HuffmanTree
mergeUntil [x] = x
mergeUntil (a : b : xs) = mergeUntil $ sort $ merge a b : xs

-- build huffman tree from text
buildTree :: String -> HuffmanTree 
buildTree = mergeUntil . map makeLeaf . charCount

-- ENCODE TEXT --

-- encode a single char
-- TODO: discover best way to trow exeption when char not in tree
encodeChar :: Char -> HuffmanTree -> [Bool] -> [Bool]
encodeChar a (Node left center right) acc =  if not $ elem a $ snd center
    then [False] --jogar exception aqui 
    else if elem a $ chars left 
        then encodeChar a left (acc++[False])
        else encodeChar a right (acc++[True])  
encodeChar a (Leaf _) acc = acc

encodeText :: String -> HuffmanTree -> [Bool]
encodeText (x:xs) htree = (encodeChar x htree []) ++ (encodeText xs htree)
encodeText _ _ = []

-- DECODE TEXT --
decodeChar :: [Bool] -> HuffmanTree -> (Char, [Bool])
decodeChar (x:xs) (Node left center right) = if x
    then decodeChar xs right
    else decodeChar xs left
decodeChar x (Leaf (_, a)) = (a !! 0, x)

decodeText :: [Bool] -> HuffmanTree -> String
decodeText [] htree = ""
decodeText x htree = [a] ++ decodeText b htree
    where (a, b) = decodeChar x htree

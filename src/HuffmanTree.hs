module HuffmanTree where

import Control.Monad
import Data.Binary

data HuffmanTree = Leaf (Int, [Char])
                   | Node (HuffmanTree) (Int, [Char]) (HuffmanTree)
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

  -- get = do t <- get :: Get Word8
  --          case t of
  --               0 -> do a <- get
  --                       return (Leaf a)
  --               1 -> do t1 <- get
  --                       s1 <- get
  --                       s2 <- get
  --                       t2 <- get
  --                       return (Node t1 (s1, s2) t2)
  --
  get = do tag <- getWord8
           case tag of
                0 -> liftM Leaf get
                1 -> liftM3 Node get get get

-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES:
 -}
characterCounts :: String -> Table Char Int
characterCounts s = characterCountsAux s (Table.empty)
  where
    characterCountsAux :: String -> Table Char Int -> Table Char Int
    characterCountsAux [] table = table
    characterCountsAux (x:xs) table = characterCountsAux (newlist) (Table.insert table (x) (count)) --
        where
          newlist = filter (/= x) (xs) -- removes all letters x from xs
          count = (length (x:xs))  - (length (newlist)) -- compares length of xs with newlist


-- modify and add comments as needed
data HuffmanTree = Leaf Char Int | Node Int (HuffmanTree) (HuffmanTree) deriving (Show, Read, Eq, Ord)

{- huffmanTree t
   PRE:  t maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLES:
 -}

huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = fst $ fst (PriorityQueue.least (huffStep (huffTreeAux t)))
  where
    huffTreeAux :: Table Char Int -> PriorityQueue HuffmanTree
    huffTreeAux table = Table.iterate table huffTrance PriorityQueue.empty
      where
        huffTrance :: PriorityQueue HuffmanTree -> (Char, Int) ->  PriorityQueue HuffmanTree
        huffTrance pq (a, b) = PriorityQueue.insert pq ((Leaf a b), b)

    huffStep :: PriorityQueue HuffmanTree -> PriorityQueue HuffmanTree
    huffStep pq
      | is_empty pq1 = pq
      | otherwise = huffStep (PriorityQueue.insert pq2 (Node (newPrio) (t1) (t2), newPrio))
       where
         ((t1, prio1), pq1) = least pq
         ((t2, prio2), pq2) = least pq1
         newPrio = prio1 + prio2
{- codeTable h
 RETURNS: a table that maps each character in h to its Huffman code
 EXAMPLES:
-}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = codeTableAux h Table.empty []
 where
  codeTableAux :: HuffmanTree -> Table Char BitCode -> BitCode -> Table Char BitCode
  codeTableAux (Leaf char int) bctable bc = Table.insert bctable char bc
  codeTableAux (Node int h1 h2) bctable bc = codeTableAux h1 (codeTableAux h2 bctable (bc ++ [True])) (bc ++ [False])

{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s = (huffTree, bitCode s)
  where
    huffTree = huffmanTree (characterCounts s)
    table = codeTable huffTree
    bitCode [] = []
    bitCode (x:xs) = let (Just bits) = (Table.lookup table x) in bits ++ (bitCode xs)

{- 1. characterCounts
characterCounts
 2. huffTree
 3. codeTable
 4. go through string and add bitCode
 5. return HuffTree and bitcode  -}




{- decompress h bits

   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress h [] = []
decompress h bits = letter:(decompress h remainingBits)
    where
      (letter, remainingBits) = decompressAcc h bits

decompressAcc :: HuffmanTree -> BitCode -> (Char, BitCode)
decompressAcc (Node n (falset) (truet)) (x:xs)
  | x == False = decompressAcc falset xs
  | otherwise = decompressAcc truet xs
decompressAcc (Leaf char _) bits = (char, bits)



--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]

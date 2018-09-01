module Test where

import           Nat
import           System.Random (newStdGen, randomRs)
import           Tree          (FoldTree (..), Tree (..))

tOrder = (5,2,10)

tBits = [15..17]

tFirstNat = fromInteger (5) :: Nat
tSecondNat = fromInteger (7) :: Nat

tContains = [[1..5], [2,0], [3,4]]

tList3 = [1..3] :: [Int]

tTree = Node 3 (Node 1 Leaf Leaf) $ Node 66 (Node 4 Leaf Leaf) Leaf

tFold = Tree 3 (Tree 1 Empty Empty) $ Tree 66 (Tree 4 Empty Empty) Empty


tList5 = [1..5]

tStr = "abc"

tCollect = [1..8]

passTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
            , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
            , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
            ]

mustFail  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]

advancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]
advancedMustFail = ["1+1", "++1", "-+1", "+-1", "1 + 1"]

tMerge = [2,1,0,3,10,5]

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

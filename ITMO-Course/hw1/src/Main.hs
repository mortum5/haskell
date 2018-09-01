module Main where

import           Block1      (contains, highestBit, order3, smartReplicate)
import           Block2      (collectEvery, mergeSort, removeAt, stringSum)
import           Block3      (Day (..), afterDay, daysToParty, isWeekend, nextDay)
import           Game        (genCreature, genMonsters, gloriousBattle)
import           MaybeConcat (maybeConcat)
import           Nat         (Nat (..), toInteger')
import           SplitOn     (splitOn)
import           Test
import           Tree        (Tree (..), count, delete, find, insert, isEmpty,
                              verticalPrint)

main :: IO ()
main = do
    print $ order3 tOrder
    print $ map highestBit tBits
    print $ smartReplicate tList3
    print $ contains 3 tContains
    print $ removeAt 1 tList3
    print $ removeAt 10 tList3
    print $ removeAt 3 tList5
    print $ removeAt 2 tStr
    print $ collectEvery 3 tCollect
    print $ map stringSum passTests
    print $ map stringSum advancedTests
    print $ mergeSort tMerge
    example <- randomIntList 5 (-10) 10
    print $ mergeSort example
    print $ nextDay Sunday
    print $ afterDay 4 Friday
    print $ isWeekend Monday
    print $ daysToParty Saturday
    print $ gloriousBattle (genCreature "Jeron" True 13) (genMonsters 5 9)
    print $ tFirstNat + tSecondNat
    print $ tFirstNat * tSecondNat
    print $ tSecondNat - tFirstNat
    print $ (fromInteger (3) :: Nat)
    print $ toInteger' tSecondNat
    print $ tFirstNat > tSecondNat
    print $ tFirstNat == tSecondNat
    putStrLn $ verticalPrint tTree
    print $ isEmpty tTree
    print $ count tTree
    print $ find tTree 13
    putStrLn $ verticalPrint $ insert tTree 13
    print $ foldr (:) [] tFold
    print $ splitOn '/' "path/to/file"
    print $ maybeConcat [Just [1,2,3], Nothing, Just [4,5]]

{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE TemplateHaskell        #-}
module Game where

import           Control.Lens
import           Control.Lens.Getter
import           System.Random       (Random, mkStdGen, randomRs)

randomList :: Random a => Int -> Int -> a -> a -> [a]
randomList seed n from to = take n (randomRs (from, to) (mkStdGen seed))

defaultRandom :: (Num a, Random a) => Int -> [a]
defaultRandom seed = randomList seed 8 0 6

genName :: Int -> [a] -> a
genName seed list = list !! head (randomList seed 1 0 (length list - 1))
data TypeEquip
    = Armor
    | Braces
    | Shoe
    | Greave
    | Weapons
    | Helmet
    | Gloves
    deriving (Show, Eq, Ord, Enum)

data Player = Player
    { _name         :: String
    , _typeOfPlayer :: Bool
    , _health       :: Int
    , _attack       :: Int
    , _defense      :: Int
    , _equipment    :: [Equipment]
    } deriving (Show)

data Equipment = Equipment
    { _name        :: String
    , _typeOfEquip :: TypeEquip
    , _health      :: Int
    , _attack      :: Int
    , _defense     :: Int
    } deriving (Show)

makeFieldsNoPrefix ''Player
makeFieldsNoPrefix ''Equipment

namesOfMonsters = ["Orc", "Dragon", "Rat", "Goblin", "Elf", "Gnom", "Elemental"]


types = [Armor .. Gloves]

listOfEmotions =
    ["Fear", "Anger", "Sadness", "Trust", "Suffering", "Anxiety", "Despair", "Negation"]

type Result = Player

type Monster = Player

-- Функция Бой в который первый боец атакует второго, с проверкой, что здоровье не равно нулю, вызывает самого себя с изменением аргументов
genCreature :: [Char] -> Bool -> Int -> Player
genCreature nameOfCreature creatureType seed = updateEquip $
    Player
    { _name = check nameOfCreature
    , _typeOfPlayer = creatureType
    , _health = 6
    , _attack = rand !! 1
    , _defense = rand !! 2
    , _equipment = genEquipment rand seed
    }
  where
    rand = defaultRandom seed
    check name
        | creatureType = name
        | otherwise = genName seed namesOfMonsters


change :: (HasEquipment s [a], Num b) => s -> ((b -> Const b b) -> a -> Const b a) -> ASetter s t b b -> t
change p g s = over s (+ (getFields p g)) p

updateEquip :: (HasDefense t a, HasDefense a3 a, HasAttack t a1, HasAttack a3 a1, HasHealth t a2, HasHealth a3 a2, Num a2, Num a, Num a1, HasEquipment t [a3]) => t -> t
updateEquip p = change (change (change p health health) attack attack) defense defense

getFields :: HasEquipment s [a] => s -> ((a1 -> Const a1 a1) -> a -> Const a1 a) -> a1
getFields p f = p ^. equipment . to (!! 0) . f


addEquip :: HasEquipment t [a] => t -> a -> t
addEquip p t = over equipment (t :) p

genEquipment :: [Int] -> Int -> [Equipment]
genEquipment rand seed =
    [ Equipment
      { _name = show (genName seed types) ++ " of " ++ genName seed listOfEmotions
      , _typeOfEquip = genName seed types
      , _health = rand !! 5
      , _attack = rand !! 6
      , _defense = rand !! 7
      }
    ]

genMonsters :: Int -> Int -> [Player]
genMonsters from to = map (genCreature "" False) [from .. to]

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle p [] = p
gloriousBattle p (m:ms) =
    if checkHealth p1 p1
        then getTrophies p1 m ms
        else p1
  where
    p1 = fight (heal p) m True
    getTrophies p1 m ms = gloriousBattle (addEquip p1 (m ^. equipment . to (!! 0))) ms

heal :: Player -> Player
heal p = set health (12) p

checkHealth :: Player -> Player -> Bool
checkHealth p1 p2 = (p1^.health > 0) && (p2^.health > 0)

strike :: Player -> Player -> Player
strike first second
    | first^.attack < second^.defense && second^.attack < first^.defense =
        error "Infinit fight"
    | first^.attack <= second^.defense = second
    | otherwise = over health (+ (second^.defense - first^.attack)) second
--
fight :: Player -> Player -> Bool -> Player
fight first second flag
    | checkHealth first second = fight (strike first second) first (not flag)
    | otherwise =
        if flag
            then first
            else second

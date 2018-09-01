module Block3
    ( Day(..)
    , nextDay
    , afterDay
    , isWeekend
    , daysToParty
    ) where

data Day
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq, Enum, Bounded)

nextDay :: Day -> Day
nextDay d
    | d == maxBound = minBound
    | otherwise = succ d

afterDay :: Int -> Day -> Day
afterDay n d = iterate nextDay d !! n

isWeekend :: Day -> Bool
isWeekend d = d == Saturday || d == Sunday

daysToParty :: Day -> Int
daysToParty = count 0
  where
    count i d
        | d == Friday = i
        | otherwise = count (i + 1) (nextDay d)

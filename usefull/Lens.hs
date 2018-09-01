#!/usr/bin/env stack
-- stack ghci
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Lens where

import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

type Lens' a b = forall f . (Functor f) => (b -> f b) -> (a -> f a)
type Traversal' a b = forall f . (Applicative f) => (b -> f b) -> (a -> f a)

traversed :: Traversal' [a] a


data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)

-- score :: Lens' Game Int
-- score = lens _score (\game v -> game { _score = v })

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point

initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }

-- newState <- execStateT strike initialState
-- newState^.boss.health

strike :: StateT Game IO ()
strike = do
    lift $ putStrLn "*shrink*"
    bossHP -= 10

bossHP :: Lens' Game Int
bossHP = boss.health

fireBreath :: StateT Game IO ()
fireBreath = do
    lift $ putStrLn "*rawr*"
    units.traversed.(around target 1.0).health -= 3

partyHP :: Traversal' Game Int
partyHP = units.traversed.healt

retreat :: StateT Game IO ()
retreat = do
    lift $ putStrLn "Retreat!"
    zoom (units.traversed.position) $ do
        x += 10
        y += 10

toListOf :: Traversal' a b -> a -> [b]
-- toListOf partyHP newState  ~ ^..

around :: Point -> Double -> Traversal' Unit Unit
around center radius = filtered (\unit -> (unit^.position.x - center^.x)^2 + (unit^.position.y - center^.y)^2 < radius^2 )

battle :: StateT Game IO ()
battle = do
    forM_ ["Take that!", "and that!", "and that!"] $ \taunt -> do
        lift $ putStrLn taunt
        strike
        fireBreath (Point 0.5 1.5)
        replicateM_ 3 $ do
            retreat
            zoom (boss.position) $ do
                x += 10
                y += 10

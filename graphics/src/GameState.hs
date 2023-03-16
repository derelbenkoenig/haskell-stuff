{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}

module GameState where

import TimeStep

data GameState t a = GameState {
    desiredTime :: t,
    updatedTime :: t,
    deltaTime :: t,
    otherData :: a
} deriving (Functor, Eq, Show)

makeInitialGameState :: TimeValue t => a -> GameState t a
makeInitialGameState a = GameState {
    desiredTime = 0,
    updatedTime = 0,
    deltaTime = 1/60,
    otherData = a
}

emptyInitialGameState :: TimeValue t => GameState t ()
emptyInitialGameState = makeInitialGameState ()

stepState :: (TimeValue t, TimeSteppable a) => GameState t a -> GameState t a
stepState gstate@GameState{..} =
    if canStepState gstate
        then gstate {updatedTime = updatedTime + deltaTime,
                     otherData = timeStep deltaTime otherData}
        else gstate

canStepState :: TimeValue t => GameState t a -> Bool
canStepState GameState{..} = updatedTime + deltaTime < desiredTime


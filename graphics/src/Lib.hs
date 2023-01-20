{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import SDL
import qualified SDL.Time as Time
import qualified SDL.Video as Video
import Control.Monad (unless)

type TimeValue t = (Ord t, Fractional t)

data GameState t a = GameState {
    desiredTime :: t,
    updatedTime :: t,
    deltaTime :: t,
    otherData :: a
} deriving (Functor, Eq, Show)

initialGameState :: TimeValue t => GameState t ()
initialGameState = GameState {
    desiredTime = 0,
    updatedTime = 0,
    deltaTime = 1/60,
    otherData = ()
}

class TimeSteppable a where
    timeStep :: forall t. TimeValue t => t -> a -> a

instance TimeSteppable () where
    timeStep _ = id

stepState :: (TimeValue t, TimeSteppable a) => GameState t a -> GameState t a
stepState gstate@GameState{..} =
    if canStepState gstate
        then gstate {updatedTime = updatedTime + deltaTime,
                     otherData = timeStep deltaTime otherData}
        else gstate

canStepState :: TimeValue t => GameState t a -> Bool
canStepState GameState{..} = updatedTime + deltaTime < desiredTime

runGame :: IO ()
runGame = do
    initializeAll
    let windowCfg = defaultWindow {
        -- TODO I have no idea if I need VulkanContext
        windowGraphicsContext = Video.VulkanContext
    }
    window <- createWindow "Playing with graphics in Haskell" windowCfg
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer initialGameState
    destroyWindow window

appLoop :: Renderer -> GameState Double () -> IO ()
appLoop renderer gameState = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events
    curTime <- Time.time
    let newGameState = until (not . canStepState) 
                        stepState (gameState{desiredTime = curTime})
    rendererDrawColor renderer $= V4 0 0 255 255
    clear renderer
    present renderer
    unless qPressed (appLoop renderer newGameState)

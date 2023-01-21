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

makeInitialGameState :: TimeValue t => a -> GameState t a
makeInitialGameState a = GameState {
    desiredTime = 0,
    updatedTime = 0,
    deltaTime = 1/60,
    otherData = a
}

emptyInitialGameState :: TimeValue t => GameState t ()
emptyInitialGameState = makeInitialGameState ()

-- TODO this may not be sufficient as the update may depend on both the current
-- time and time delta, not just the delta
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

runGame :: (TimeValue t, TimeSteppable a) => GameState t a -> IO ()
runGame initialGameState = do
    initializeAll
    let windowCfg = defaultWindow {
        -- TODO I have no idea if I need VulkanContext
        windowGraphicsContext = Video.VulkanContext
    }
    window <- createWindow "Playing with graphics in Haskell" windowCfg
    renderer <- createRenderer window (-1) defaultRenderer
    curTime <- Time.time
    appLoop renderer (initialGameState{updatedTime = curTime})
    destroyWindow window

appLoop :: (TimeValue t, TimeSteppable a) => Renderer -> GameState t a -> IO ()
appLoop renderer gameState = do
    events <- pollEvents
    let qPressed = any isQPress events
    curTime <- Time.time
    let newGameState = until (not . canStepState) 
                        stepState (gameState{desiredTime = curTime})
    rendererDrawColor renderer $= V4 0 0 255 255
    clear renderer
    present renderer
    unless qPressed (appLoop renderer newGameState)

isQPress :: Event -> Bool
isQPress event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        _ -> False

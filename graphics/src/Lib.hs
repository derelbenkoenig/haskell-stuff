{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
  module GameState,
  runGame
) where

import SDL
import qualified SDL.Time as Time
import qualified SDL.Video as Video
import Control.Monad (unless)
import TimeStep
import GameState

runGame :: (TimeValue t, TimeSteppable a) => GameState t a -> IO ()
runGame initialGameState = do
    initializeAll
    let windowCfg = defaultWindow {
        -- TODO I have no idea if I am gonna need VulkanContext
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


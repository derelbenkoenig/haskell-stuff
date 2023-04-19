module Main (main) where

import Lib
import Rectangle
import SDL

main :: IO ()
main = runGame $ makeInitialGameState $
    RectanglePlayer (Position (V2 20 20)) undefined undefined (V2 20 20)


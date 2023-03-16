
module Rectangle where

import GameState
import TimeStep
import Motion2
import SDL.Vect

data RectanglePlayer a = RectanglePlayer {
    position :: Position a,
    velocity :: Velocity a,
    acceleration :: Acceleration a,
    dimensions :: V2 a
}

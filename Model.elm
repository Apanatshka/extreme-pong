{- |
Module      :  Model
Description :  Implementation of Extreme Pong.
Copyright   :  (c) Jeff Smits
License     :  GPL-3.0

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Model where

-----------
-- MODEL --
-----------

-- Dimensions of the playing field
-- (width, height) :: (Int, Int)
fieldSize = (600, 400)
-- Half the field dimensions
-- (width, height) :: (Float, Float)
halfField = ((fst fieldSize) / 2, (snd fieldSize) / 2)

-- distance between paddle and playing field edge
paddleDist = 10

{-
Data for a paddle on the field
Paddle y-pos (on the surface of the paddle in the center)

  ||
  ||
  |* (y-pos)
  ||
  ||

-}
data Paddle = Paddle Float
-- Dimensions of the paddle
-- (width, height) :: (Int, Int)
paddleSize = (8, 46)

defaultPaddle = Paddle $ snd halfField

-- Data for the ball
-- Ball position velocity
data Ball = Ball (Float, Float) (Float, Float)
ballRadius = 8

defaultBall = Ball halfField (100, 100)

-- The score of the players
data Score = Score Int Int

-- The state of the game
data GameState = Paused | Playing

-- All the data needed to model the game
data State = State GameState Score Ball Paddle Paddle

-- The State to start with
defaultGame = State Paused
                    (Score 0 0)
                    defaultBall
                    defaultPaddle
                    defaultPaddle

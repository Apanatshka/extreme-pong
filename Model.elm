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

-- All the data needed to model the game
data State = State Score Ball Paddle Paddle

-- The State to start with
defaultGame = State (Score 0 0)
                    defaultBall
                    defaultPaddle
                    defaultPaddle

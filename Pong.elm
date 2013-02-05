{- |
Module      :  Pong
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

module Pong where

import Either
import Graphics

-------------
-- GENERAL --
-------------

{-
Sadly the compiler can't handle a tuple with the default ball
 velocity in the definition of defaultBall when these are defined. 

type Position = (Float, Float)
type Velocity = (Float, Float)
-}

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

----------------
-- CONTROLLER --
----------------

-- All input from the user
-- Spacebar, left player and right player values
-- where values are -1, 0 or 1
data UserInput = UserInput Bool Int Int

defaultUserInput = UserInput False 0 0

-- All input and the time since the last input
data Input = Input Float UserInput

defaultInput = Input 0 defaultUserInput

{-
Takes the State to the next State on an Input
Sadly, multiple definitions of the same function with different
 pattern-matched input just overwrite eachother in Elm, so this
 function is a little long and cluttery.

stepGame :: Input -> State -> State
-}
stepGame (Input delta (UserInput spacebar left right)) 
         (State gameState (Score ls rs) b lp rp) = 
  let sc = (Score ls rs)
  in if spacebar then case gameState of
    Paused  -> State Playing sc b lp rp
    Playing -> State Paused  sc b lp rp
  else case gameState of
    Paused  -> State Paused  sc b lp rp
    Playing -> let
      bvec = ballVector delta b
      -- try to collide with the paddles, walls, and paddles again
      b' = collide bvec $ [collidePaddles lp rp, 
                collideFieldWalls, collidePaddles lp rp]
      fxlb = 0
      fxub = fst fieldSize
      (ls', rs', s', b'') = case b' of Ball (bx',_) _ ->
        -- sadly, the multiway if doesn't seem to be working...
        if bx' < fxlb then (ls,   rs+1, Paused,  defaultBall) else if
           bx' > fxub then (ls+1, rs,   Paused,  defaultBall) else
           {- otherwise -} (ls,   rs,   Playing, b')
    in State s' (Score ls' rs') b''
      (stepPaddle delta left lp) (stepPaddle delta right rp)

{-
Takes a time delta, a direction and Paddle and
 returns a paddle in that direction.
stepPaddle :: Float -> Int -> Paddle -> Paddle
-}
stepPaddle delta dir (Paddle y) = let
    -- paddle half height, lower and upper bound
    phh = snd paddleSize / 2
    plb = 0               + phh
    pub = (snd fieldSize) - phh
  -- negate the direction because Graphics points the y-axis down
  in Paddle $ clamp plb pub $ y - (toFloat dir) * delta * 100

-- Data for the ball within a computation step
-- Ball position-vector velocity
data StepBall = StepBall ((Float, Float),(Float,Float)) (Float, Float)

{-
ballVector returns the vector (defined by two points) from the current
 position to the next position using the time difference
 and the velocity of the Ball. 

ballVector :: Float -> Ball -> StepBall
-}
ballVector d (Ball (x, y) (vx, vy)) = 
  StepBall ((x, y), (x+d*vx, y+d*vy)) (vx, vy)

{-
collide takes a vector (defined by two points) 
 and a list of collision functions.
It calls each function until the vector doesn't collide with that
 function anymore.
Then it returns the position pointed to by the vector. 

collide :: StepBall
        -> [StepBall
            -> Maybe StepBall]
        -> (Float, Float)
-}
collide sb l = case l of
  Cons h t -> case h sb of
    Just sb' -> collide sb' (h:t)
    Nothing  -> collide sb  t
  Nil      -> case sb of
    StepBall (_,pos) vel -> Ball pos vel

{-
collidePaddles :: Paddle
               -> Paddle
               -> StepBall
               -> Maybe StepBall
-}
collidePaddles (Paddle lpy) (Paddle rpy) sb = 
  let distFromEdge = paddleDist + fst paddleSize
  in case collidePaddle (Left (distFromEdge, lpy)) sb of
    Just sb -> Just sb
    Nothing -> collidePaddle (Right ((fst fieldSize) - distFromEdge, rpy)) sb

{-
Checks if the vector (defined by points 1 and 2) crosses the paddle
 and if so returns a vector from the collision point (cp)
 to the reflection point (rp).

(2) *      * (rp)                             (rp) *     * (2)
     \    /                                         \   /
      || /                                           \ ||
      ||* (cp)                                   (cp) *||
      || \                                           / ||
      ||  * (1)                                 (1) *  ||
      ||                                               ||

collidePaddle :: Either (Float, Float)
              -> StepBall
              -> Maybe StepBall
-}
collidePaddle p (StepBall ((x1,y1),(x2,y2)) (vx,vy)) =
  let (cpx, condx, py) = case p of
        Left  (px,py) -> let cpx = px+ballRadius
          in (cpx, x2 <= cpx && cpx <= x1, py)
        Right (px,py) -> let cpx = px-ballRadius
          in (cpx, x1 <= cpx && cpx <= x2, py)
      cpy = y1 + (x1-cpx)/(x2-x1) * (y2-y1)
      rp = (cpx - (x2-cpx), y2)
      -- paddle lower and upper bound
      plb = py - (snd paddleSize / 2) - ballRadius
      pub = py + (snd paddleSize / 2) + ballRadius
      condy = plb <= cpy && cpy <= pub
  in if condx && condy
    then Just $ StepBall ((cpx,cpy),rp) (0-vx,vy)
    else Nothing

{-
collideFieldWalls :: StepBall
                  -> Maybe StepBall
-}
collideFieldWalls sb =
  case collideFieldWall (Left 0) sb of
    Just sb' -> Just sb'
    Nothing  -> collideFieldWall (Right $ snd fieldSize) sb

{-
Checks if the vector (defined by points 1 and 2) crosses
 a field wall and if so returns a vector
 from the collision point (cp) to the reflection point (rp).

      (2) *
         /
 ___________
  (cp) *
      / \
 (1) *   \
     (rp) *

collideFieldWalls :: Either Float Float
                  -> StepBall
                  -> Maybe StepBall
-}
collideFieldWall wallBound (StepBall ((x1,y1),(x2,y2)) (vx,vy)) =
  let (cpy, cond) = case wallBound of
        Left  wlb -> (wlb+ballRadius, y2  <= (wlb+ballRadius))
        Right wub -> (wub-ballRadius, (wub-ballRadius) <= y2 )
      cpx = x1 + (y1-cpy)/(y2-y1) * (x2-x1)
      rp  = (x1, cpy - (y2-cpy))
  in if cond then Just $ StepBall ((cpx,cpy),rp) (vx,0-vy) 
             else Nothing

----------
-- VIEW --
----------

-- Creates a scoreboard
-- scoreBoard :: Int -> Bool -> Score -> Element
scoreBoard w playing (Score lp rp) =
  let code = text . monospace . toText
      stack top bottom = flow down [ code " ", code top, code bottom ]
      msg = width w . centeredText . monospace . toText $
        (if playing 
          then "PLAYING\n(press Spacebar to pause)" 
          else "PAUSED\n(press Spacebar to play)")
      board = flow right 
        [ stack "W" "S"
        , spacer 20 1
        , text . Text.height 4 . toText $ show lp
        , spacer 60 1
        , text . Text.height 4 . toText $ show rp
        , spacer 20 1
        , stack "&uarr;" "&darr;" ]
      score = container w (heightOf board) midTop board
  in score `above` msg

-- Display function of a state
-- display :: (Int, Int) -> State -> Element
display (w,h) (State gs sc (Ball pos _) (Paddle lp) (Paddle rp)) = let
    (fx,fy) = fieldSize
    fieldGreen = rgb 0 102 0
    lineYellow = rgb 255 255 240
    even n = 2 * (n `div` 2)
    field = rect fx fy halfField
    midCircle = circle (fx/20) halfField
    -- the 0.5+ is to fix an anti-aliasing issue
    midLine = segment (0.5+fst halfField,fy) (0.5+fst halfField,0)
    (px,py) = paddleSize
    lDistFromEdge = paddleDist + (px/2)
    rDistFromEdge = fx - lDistFromEdge
  in flow down
    [ scoreBoard w (gs == Playing) sc
    , container w fy midTop $ color lineYellow $ collage fx fy 
      [ filled   fieldGreen field
      , outlined lineYellow field
      , solid    lineYellow midLine
      , outlined lineYellow midCircle
      , filled white (circle ballRadius pos)
      , filled white (rect px py (lDistFromEdge,lp))
      , filled white (rect px py (rDistFromEdge,rp))
      ]
    , width w . rightedText . monospace . toText (
      (show . round) (second/delta) ++ "FPS")
    ]

--------
-- IO --
--------

-- Time signal
-- delta :: Signal Float
delta = lift inSeconds $ fps 60

-- foldps is a signal which is dependant on a past (hidden) state
-- foldps :: (a -> s -> (a,s)) -> s -> Signal a -> Signal a
foldps  f s  sig = lift fst $ foldp  f s  sig
foldps' f sf sig = lift fst $ foldp' f sf sig

-- Input signal
-- userInput :: Signal UserInput
userInput = lift3 UserInput 
  Keyboard.space
  (lift .y Keyboard.wasd)
  (lift .y Keyboard.arrows)

{-
Combination of input and time signals
The let expession using foldps makes sure the Keyboard.space signal
 turns into a signal which turns true once when the Keyboard.Space
 signal goes from false to true. This can only be handled here because
 of the sampleOn function used to define sig.

input :: Signal Input
-}
input = let f (Input d (UserInput sp' l r)) (_, sp) =
              if not sp then (Input d (UserInput sp' l r),    sp')
                        else (Input d (UserInput False l r),  sp')
            s = (defaultInput, False)
            sig = sampleOn delta $ lift2 Input delta userInput
        in foldps f s sig

-- State signal
-- gameState :: Signal State
state = foldp stepGame defaultGame input

{- 
Lift the display of the State over a Signal of
the window dimensions and a Signal of the State.
-}
main = lift2 display Window.dimensions state
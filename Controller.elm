{- |
Module      :  Controller
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

module Controller where

import Model
import Either
import Line

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
 function is a little long and cluttered.
-}
stepGame : Input -> State -> State
stepGame (Input delta (UserInput spacebar left right)) 
         (State gameState (Score ls rs) b lp rp) = 
  let sc = Score ls rs
  in if spacebar then case gameState of
    Paused  -> State Playing sc b lp rp
    Playing -> State Paused  sc b lp rp
  else case gameState of
    Paused  -> State Paused  sc b lp rp
    Playing -> let
      bvec = stepBall delta b
      -- try to collide with the paddles, walls, and paddles again
      b' = collide bvec $ [collidePaddles lp rp, 
                collideFieldWalls, collidePaddles lp rp]
      fxlb = 0
      fxub = fst fieldSize
      (gs', ls', rs', b'') = case b' of Ball (bx',_) _ ->
        -- sadly, the multiway if doesn't seem to be working...
        if bx' < fxlb then (Paused,  ls,   rs+1, defaultBall) else if
           bx' > fxub then (Paused,  ls+1, rs,   defaultBall) else
           {- otherwise -} (Playing, ls,   rs,   b')
    in State gs' (Score ls' rs') b''
      (stepPaddle delta left lp) (stepPaddle delta right rp)

{-
Takes a time delta, a direction and Paddle and
 returns a paddle in that direction.
-}
stepPaddle : Float -> Int -> Paddle -> Paddle
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
stepBall returns the StepBall with a vector (defined by two points)
 from the current position to the next position using the time
 difference and the velocity of the Ball. 
-}
stepBall : Float -> Ball -> StepBall
stepBall d (Ball (x, y) (vx, vy)) = 
  StepBall ((x, y), (x+d*vx, y+d*vy)) (vx, vy)

{-
collide takes a vector (defined by two points) 
 and a list of collision functions.
It calls each function until the vector doesn't collide with that
 function anymore.
Then it returns the position pointed to by the vector. 
-}
collide : StepBall
        -> [StepBall -> Maybe StepBall]
        -> (Float, Float)
collide sb l = case l of
  h::t -> case h sb of
    Just sb' -> collide sb' (h::t)
    Nothing  -> collide sb  t
  []   -> case sb of
    StepBall (_,pos) vel -> Ball pos vel

{-
-}
collidePaddles : Paddle
               -> Paddle
               -> StepBall
               -> Maybe StepBall
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

-}
collidePaddle : Either (Float, Float) (Float, Float)
              -> StepBall
              -> Maybe StepBall
collidePaddle p (StepBall (p1,(x2,y2)) (vx,vy)) =
  let plb y = y - (snd paddleSize / 2) - ballRadius
      pub y = y + (snd paddleSize / 2) + ballRadius
      (mCp, pp) = case p of
        Left  (px,py) -> let
            paddSegm = verLineSegment (plb py) (pub py) $ px+ballRadius
            ballSegm = Segment p1 (x2,y2)
          in case intersectSS paddSegm ballSegm of
            Just (C CHRight2Left _,cp) -> (Just cp,(px,py))
            _  -> (Nothing, (px,py))
        Right (px,py) -> let
            paddSegm = verLineSegment (plb py) (pub py) $ px-ballRadius
            ballSegm = Segment p1 (x2,y2)
          in case intersectSS paddSegm ballSegm of
            Just (C CHLeft2Right _,cp) -> (Just cp, (px,py))
            _ -> (Nothing, (px,py))
  in case mCp of
    Just (cpx,cpy) -> let rp  = (cpx - (x2-cpx), y2)
                          sb = (StepBall (p1,(x2,y2)) (vx,vy))
                          bA = ballAngle sb
                          bA' = switchHorVer bA
                          pRAA = paddleReflectionAxisAngle $ cpy - (snd pp)
                          rA = reflectionAngle pRAA bA'
                          l = sqrt $ ((x2-cpx)^2)+((y2-cpy)^2)
                          cp = (cpx,cpy)
                          rp' = reflectionPoint rA cp l
                          vl = sqrt $ vx^2 + vy^2
                          v' = relReflPoint rA vl
                      in Just $ StepBall ((cpx,cpy),rp') v'
    _              -> collidePaddleSides pp (StepBall (p1,(x2,y2)) (vx,vy))

{-
the following code it for arkenoid paddle behaviour. It's ad-hoc right now,
 I will refactor it later. 
-}
ballAngle : StepBall -> Float
ballAngle (StepBall ((x1,y1),(x2,y2)) _) =
  let dx = x2-x1 -- both positive means going down and to the right
      dy = y2-y1 -- both positive means going down and to the right
  in atan2 dy dx

switchHorVer : Float -> Float
switchHorVer d = 0.5 * pi - d

paddleReflectionAxisAngle : Float -> Float
paddleReflectionAxisAngle cpyRelToPadMid = (90 - cpyRelToPadMid) / 180 * pi

reflectionAngle : Float -> Float -> Float
reflectionAngle padReflAxisAngle ballAngle =
  let dAngle = padReflAxisAngle - ballAngle
  in ballAngle + 2 * dAngle

reflectionPoint : Float -> (Float,Float) -> Float -> (Float,Float)
reflectionPoint reflAngle (cpx,cpy) length =
  let (dx,dy) = relReflPoint reflAngle length
  in (cpx+dx,cpy+dy)

relReflPoint : Float -> Float -> (Float,Float)
relReflPoint reflAngle length =
  let dx = 0 - length * sin reflAngle
      dy = 0 - length * cos reflAngle
  in (dx,dy)


collidePaddleSides : (Float,Float)
                   -> StepBall
                   -> Maybe StepBall
collidePaddleSides p sb = 
  let paddletopside = collidePaddleSide (Left p) sb
  in case paddletopside of
    Just _   -> paddletopside
    Nothing  -> collidePaddleSide (Right p) sb

{-
Hitting the side in stead of the front of the paddle should bounce
 the ball off to the side. Ineffective for gameplay, but a lot better
 looking than having the ball go through the side of the paddle. 

 (rp) *   * (1)
       \ /
        * (cp)
       /||
  (2) * ||
        |+ (px,py)
        ||
        ||

Left (px,py) means the left paddle side when the scene is turned 90 degrees
 counter-clockwise (so the upper side). Px and py still mean the same as in
 collidePaddle. 
-}
collidePaddleSide : Either (Float, Float) (Float, Float)
                  -> StepBall
                  -> Maybe StepBall
collidePaddleSide p (StepBall (p1,(x2,y2)) (vx,vy)) =
  let (pSx, pSy) = paddleSize
      plb x = x - pSx - ballRadius
      prb x = x + ballRadius
      mCp = case p of
        -- Left means lower side, Right means upper side
        Left (px,py)  -> let 
            paddSegm = horLineSegment (plb px) (prb px) $ py - (pSy/2) - ballRadius
            ballSegm = Segment p1 (x2,y2)
          in case intersectSS paddSegm ballSegm of
            Just (C _ CVTop2Bottom,cp) -> Just cp
            _ -> Nothing
        Right (px,py) -> let 
            paddSegm = horLineSegment (plb px) (prb px) $ py + (pSy/2) + ballRadius
            ballSegm = Segment p1 (x2,y2)
          in case intersectSS paddSegm ballSegm of
            Just (C _ CVBottom2Top,cp) -> Just cp
            _ -> Nothing
  in case mCp of
    Just (cpx,cpy) -> let rp  = (x2, cpy - (y2-cpy))
                      in Just $ StepBall ((cpx,cpy),rp) (vx,0-vy)
    _              -> Nothing

{-
-}
collideFieldWalls : StepBall
                  -> Maybe StepBall
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

-}
collideFieldWall : Either Float Float
                  -> StepBall
                  -> Maybe StepBall
collideFieldWall wallBound (StepBall (p1,(x2,y2)) (vx,vy)) =
  let segm = Segment p1 (x2,y2)
      mCp = case wallBound of
        Left  wlb -> case intersectLS (horLine $ wlb + ballRadius) segm of
          Just (C _ CVBottom2Top,cp) -> Just cp
          _ -> Nothing
        Right wub -> case intersectLS (horLine $ wub - ballRadius) segm of
          Just (C _ CVTop2Bottom,cp)  -> Just cp
          _ -> Nothing
  in case mCp of
    Just (cpx,cpy) ->
      let rp  = (x2, cpy - (y2-cpy))
      in Just $ StepBall ((cpx,cpy),rp) (vx,0-vy)
    Nothing        -> 
      Nothing
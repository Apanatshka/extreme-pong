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
data UserInput = UserInput Int Int

defaultUserInput = UserInput 0 0

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
stepGame (Input delta (UserInput left right)) 
         (State (Score ls rs) b lp rp) = 
  let bvec = stepBall delta b
      -- try to collide with the paddles, walls, and paddles again
      b' = collide bvec $ [collidePaddles lp rp, 
                collideFieldWalls, collidePaddles lp rp]
      fxlb = 0
      fxub = fst fieldSize
      (ls', rs', b'') = case b' of Ball (bx',_) _ ->
        -- sadly, the multiway if doesn't seem to be working...
        if bx' < fxlb then (ls,   rs+1, defaultBall) else if
           bx' > fxub then (ls+1, rs,   defaultBall) else
           {- otherwise -} (ls,   rs,   b')
    in State (Score ls' rs') b''
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
                      in Just $ StepBall ((cpx,cpy),rp) (0-vx,vy)
    _              -> collidePaddleSides pp (StepBall (p1,(x2,y2)) (vx,vy))


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
            paddSegm = horLineSegment (plb px) (prb px) $ py - pSy - ballRadius
            ballSegm = Segment p1 (x2,y2)
          in case intersectSS paddSegm ballSegm of
            Just (C _ CVTop2Bottom,cp) -> Just cp
            _ -> Nothing
        Right (px,py) -> let 
            paddSegm = horLineSegment (plb px) (prb px) $ py + pSy + ballRadius
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
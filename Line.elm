{- |
Module      :  Line
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

module Line where

--          Line DC                   + y of a point it goes through
data Line = Line DirectionCoefficient Float

--             Segment p1 as (x,y)    p2 as (x,y)
data Segment = Segment (Float, Float) (Float, Float)

--           Standard case, coefficient between -Inf and Inf exclusive
data DirectionCoefficient = DC Float
                          | DCVer Float
-- (coefficient infinite->) Vertical + x of a point it goes through

-- direction of cut
data Cut = C CutHor CutVer
data CutHor = CHLeft2Right | CHRight2Left | CHNeutral
data CutVer = CVBottom2Top | CVTop2Bottom | CVNeutral

-- horLine returns a horizontal line at the given y position
horLine : Float -> Line
horLine y = Line (DC 0) y

-- verLine returns a vertical line at the given x position
verLine : Float -> Line
verLine x = Line (DCVer x) 0

-- horLineSegment returns a horizontal line segment
horLineSegment : Float -> Float -> Float -> Segment
horLineSegment x1 x2 y = Segment (x1, y) (x2, y)

-- verLineSegment returns a vertical line segment
verLineSegment : Float -> Float -> Float -> Segment
verLineSegment y1 y2 x = Segment (x, y1) (x, y2)

isHorL : Line -> Bool
isHorL (Line dc _) = case dc of
  DC dc -> dc == 0
  _     -> False

isHorS : Segment -> Bool
isHorS (Segment (_,y1) (_,y2)) = y1 == y2

isVerL : Line -> Bool
isVerL (Line dc _) = case dc of
  DCVer _ -> True
  _       -> False

isVerS : Segment -> Bool
isVerS (Segment (x1,_) (x2,_)) = x1 == x2

-- lineFromSegment creates a Line from a Segment
lineFromSegment : Segment -> Line
lineFromSegment (Segment (x1,y1) (x2,y2)) = 
  let dx = x2-x1
      dy = y2-y1
  in if dx == 0
    then Line (DCVer x1) 0
    else let
        dc = dy/dx
        y  = y1 - dc * x1
      in Line (DC dc) y

-- intersect finds the intersection between two lines, if it exists.
intersectLL : Line -> Line -> Maybe (Float, Float)
intersectLL (Line dc1 y1) (Line dc2 y2) =
  if dc1 == dc2 || (isVerL (Line dc1 y1) && isVerL (Line dc2 y2))
    then Nothing
    else Just (case (dc1, dc2) of
      (DC c1,   DC c2  ) -> let x = (y1-y2)/(c2-c1) in (x, c1 * x + y1)
      (DC c1,   DCVer x) -> (x, c1 * x + y1)
      (DCVer x, DC c2  ) -> (x, c2 * x + y2) )

-- intersects checks if two lines intersect
intersectsLL : Line -> Line -> Bool
intersectsLL = isJust . intersect

-- betweenIncl check if subject is between left and right inclusive
betweenIncl : Float -> (Float, Float) -> Bool
betweenIncl subject (left,right) = left <= subject && subject <= right
-- betweenExcl check if subject is between left and right exclusive
betweenExcl : Float -> (Float, Float) -> Bool
betweenExcl subject (left,right) = left <  subject && subject <  right
{-
withinIncl check if subject is within the range of bound1 and bound2
 inclusive.
-}
withinIncl : Float -> (Float, Float) -> Bool
withinIncl subject (bound1,bound2) = betweenIncl subject (bound1,bound2)
                                  || betweenIncl subject (bound2,bound1)
{-
withinExcl check if subject is within the range of bound1 and bound2
 exclusive.
-}
withinExcl : Float -> (Float, Float) -> Bool
withinExcl subject (bound1,bound2) = betweenExcl subject (bound1,bound2)
                                  || betweenExcl subject (bound2,bound1)

{-
intersectLS checks if the given segment and line intersect. If they
 do, Left indicates the segment went through the line from left to
 right / bottom to top. Right indicates the complement. 
-}
intersectLS : Line -> Segment -> Maybe (Cut,(Float, Float))
intersectLS l (Segment (x1,y1) (x2,y2)) = 
  let s = Segment (x1,y1) (x2,y2)
  in case intersectLL l (lineFromSegment s) of
    Nothing -> Nothing
    Just (ipx,ipy) -> 
      let mCh = if x1 == x2 && x2 == ipx     then Just CHNeutral
            else if ipx `betweenIncl` (x1,x2) then Just CHLeft2Right
            else if ipx `betweenIncl` (x2,x1) then Just CHRight2Left
            else Nothing
          mCv = if y1 == y2 && y2 == ipy     then Just CVNeutral
            else if ipy `betweenIncl` (y1,y2) then Just CVTop2Bottom
            else if ipy `betweenIncl` (y2,y1) then Just CVBottom2Top
            else Nothing
      in case (mCh,mCv) of
        (Just ch, Just cv) -> Just (C ch cv,(ipx,ipy))
        _                  -> Nothing

-- intersectSL is intersectLS with the arguments flipped
intersectSL : Segment -> Line -> Maybe (Cut,(Float, Float))
intersectSL = flip intersectLS

-- intersectsLS  checks if a line and segment intersect
intersectsLS : Line -> Segment -> Bool
intersectsLS = isJust . intersectsLS

-- intersectsSL is intersectsLS with the arguments flipped
intersectsSL : Segment -> Line -> Bool
intersectsSL = flip intersectsLS

{-
intersectLS checks if the first and second segment intersect. If they
 do, Left indicates the first segment went through the second from
 left to right / bottom to top. Right indicates the complement. 
-}
intersectSS : Segment -> Segment -> Maybe (Cut,(Float, Float))
intersectSS (Segment (x1,y1) (x2,y2)) s2 = 
  let s = Segment (x1,y1) (x2,y2)
  in case intersectLS (lineFromSegment s) s2 of
    Nothing -> Nothing
    Just (cut,(ipx,ipy)) -> 
      if ipx `withinIncl` (x1,x2) && ipy `withinIncl` (y1,y2)
        then Just (cut,(ipx,ipy))
        else Nothing

-- intersectsSS  checks if two segments intersect
intersectsSS : Segment -> Segment -> Bool
intersectsSS = isJust . intersectsSS

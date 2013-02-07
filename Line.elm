module Line ( Line
            , Segment
            , DirectionCoefficient
            , DC
            , DCVer
            , horLine
            , verLine
            , horLineSegment
            , verLineSegment
            , isHorL
            , isHorS
            , isVerL
            , isVerS
            , lineFromSegment
            , intersectLL
            , intersectsLL
            , intersectLS
            , intersectSL
            , intersectsLS
            , intersectsSL
            , intersectSS
            , intersectsSS
            ) where

--          Line DC                   + y of a point it goes through
data Line = Line DirectionCoefficient Float

--             Segment p1 as (x,y)    p2 as (x,y)
data Segment = Segment (Float, Float) (Float, Float)

--           Standard case, coefficient between -Inf and Inf exclusive
data DirectionCoefficient = DC Float
                          | DCVer Float
-- (coefficient infinite->) Vertical + x of a point it goes through

-- horLine returns a horizontal line at the given y position
-- horLine :: Float -> Line
horLine y = Line (DC 0) y

-- verLine returns a vertical line at the given x position
-- verLine :: Float -> Line
verLine x = Line (DCVer x) 0

-- horLineSegment returns a horizontal line segment
-- horLineSegment :: Float -> Float -> Float -> Segment
horLineSegment x1 x2 y = Segment (x1, y) (x2, y)

-- verLineSegment returns a vertical line segment
-- verLineSegment :: Float -> Float -> Float -> Segment
verLineSegment y1 y2 x = Segment (x, y1) (x, y2)

-- isHorL :: Line -> Bool
isHorL (Line dc _) = case dc of
  DC dc -> dc == 0
  _     -> False

-- isHorS :: Segment -> Bool
isHorS (Segment (_,y1) (_,y2)) = y1 == y2

-- isHorL :: Line -> Bool
isVerL (Line dc _) = case dc of
  DCVer _ -> True
  _       -> False

-- isHorS :: Segment -> Bool
isVerS (Segment (x1,_) (x2,_)) = x1 == x2

-- lineFromSegment creates a Line from a Segment
-- lineFromSegment :: Segment -> Line
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
-- intersect :: Line -> Line -> Maybe (Float, Float)
intersectLL (Line dc1 y1) (Line dc2 y2) =
  if dc1 == dc2
    then Nothing
    else Just (case (dc1, dc2) of
      (DC c1,   DC c2  ) -> let x= (y1-y2)/(x2-x1) in (x, c1 * x + y1)
      (DC c1,   DCVer x) -> (x, c1 * x + y1)
      (DCVer x, DC c2  ) -> (x, c2 * x + y2) )

-- intersects checks if two lines intersect
-- intersects :: Line -> Line -> Bool
intersectsLL = isJust . intersect

-- betweenIncl check if subject is between left and right inclusive
-- betweenIncl :: Number -> Number -> Number -> Bool
betweenIncl left right subject = left <= subject && subject <= right
-- betweenExcl check if subject is between left and right exclusive
-- betweenExcl :: Number -> Number -> Number -> Bool
betweenExcl left right subject = left <  subject && subject <  right
{-
withinIncl check if subject is within the range of bound1 and bound2
 inclusive.
withinIncl :: Number -> Number -> Number -> Bool
-}
withinIncl bound1 bound2 subject = betweenIn bound1 bound2 subject
                              || betweenIn bound2 bound1 subject
{-
withinExcl check if subject is within the range of bound1 and bound2
 exclusive.
withinExcl :: Number -> Number -> Number -> Bool
-}
withinExcl bound1 bound2 subject = betweenEx bound1 bound2 subject
                              || betweenEx bound2 bound1 subject

{-
intersectLS checks if the given segment and line intersect. If they
 do, Left indicates the segment went through the line from left to
 right / bottom to top. Right indicates the complement. 

intersectLS :: Line -> Segment -> Maybe (Either (Float, Float))
-}
intersectLS l (Segment (x1,y1) (x2,y2)) = 
  let s = Segment (x1,y1) (x2,y2)
  in case intersectLL l (lineFromSegment s) of
    Nothing -> Nothing
    Just (ipx,ipy) -> 
      if betweenIncl x1 x2 ipx && betweenIncl y1 y2 ipy
        then Left $ Just (ipx,ipy) else
          if betweenIncl x2 x1 ipx && betweenIncl y2 y1 ipy
            then Right $ Just (ipx,ipy)
            else Nothing

-- intersectSL is intersectLS with the arguments flipped
-- intersectSL :: Segment -> Line -> Maybe (Either (Float, Float))
intersectSL = flip intersectLS

-- intersectsLS  checks if a line and segment intersect
-- intersectsLS :: Line -> Segment -> Bool
intersectsLS = isJust . intersectsLS

-- intersectsSL is intersectsLS with the arguments flipped
-- intersectsSL :: Segment -> Line -> Bool
intersectsSL = flip intersectsLS

{-
intersectLS checks if the first and second segment intersect. If they
 do, Left indicates the first segment went through the second from
 left to right / bottom to top. Right indicates the complement. 

intersectSS :: Segment -> Segment -> Maybe (Either (Float, Float))
-}
intersectSS s = intersectLS (lineFromSegment s)

-- intersectsLS  checks if two segments intersect
-- intersectsLS :: Segment -> Segment -> Bool
intersectsSS = isJust . intersectsSS

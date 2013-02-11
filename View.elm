{- |
Module      :  View
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

module View where

import Model

----------
-- VIEW --
----------

-- Creates a scoreboard
scoreBoard : Bool -> Int -> Score -> Element
scoreBoard pl w (Score lp rp) =
  let georgia = typeface "georgia"
      code = text . georgia . toText
      stack top bottom = flow down [ code " ", code top, code bottom ]
      msg = width w . centeredText . georgia . toText $ (
        if pl then "press Spacebar to pause" else "press Spacebar to play")
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
display : Float -> (Int, Int) -> State -> Element
display delta (w,h) (State gs sc (Ball pos _) (Paddle lp) (Paddle rp)) = let
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
    fps = (show . round) (if delta == 0 then 0 else 1/delta)
  in flow down
    [ scoreBoard (gs == Playing) w sc
    , container w fy midTop $ color lineYellow $ collage fx fy 
      [ filled   fieldGreen field
      , outlined lineYellow field
      , solid    lineYellow midLine
      , outlined lineYellow midCircle
      , filled white (circle ballRadius pos)
      , filled white (rect px py (lDistFromEdge,lp))
      , filled white (rect px py (rDistFromEdge,rp))
      ]
    , width w . rightedText . monospace . toText
        $ fps ++ " FPS"
    ]
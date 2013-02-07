module View where

import Model

----------
-- VIEW --
----------

-- Creates a scoreboard
scoreBoard : Int -> Score -> Element
scoreBoard w (Score lp rp) =
  let code = text . monospace . toText
      stack top bottom = flow down [ code " ", code top, code bottom ]
      msg = width w . centeredText . monospace . toText
        $ "press Spacebar to toggle pause"
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
display : (Int, Int) -> State -> Element
display (w,h) (State sc (Ball pos _) (Paddle lp) (Paddle rp)) = let
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
    [ scoreBoard w sc
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
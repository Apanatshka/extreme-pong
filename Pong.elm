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

-------------
-- GENERAL --
-------------

{-
Sadly the compiler can't handle a tuple with the default ball
 velocity in the definition of defaultBall when these are defined. 

type Position = (Float, Float)
type Velocity = (Float, Float)
-}

import Model
import View
import Controller

--------
-- IO --
--------

-- foldps is a signal which is dependant on a past (hidden) state
foldps : (a -> (b,s) -> (b,s)) -> (b,s) -> Signal a -> Signal b
foldps  f s  sig = lift fst $ foldp  f s  sig
foldps': (a -> (b,s) -> (b,s)) -> (a -> (b,s)) -> Signal a -> Signal b
foldps' f sf sig = lift fst $ foldp' f sf sig

-- Game pause state signal
-- playing :: Signal Bool
playing = foldps'
  (\sp (p,lastSp) -> 
    if sp && not lastSp
      then (not p, sp)
      else (p, sp) )
  (\sp -> (False, False) )
  Keyboard.space

-- Time signal
-- delta :: Signal Float
delta = inSeconds <~ fpsWhen 60 playing

-- Input signal
-- userInput :: Signal UserInput
userInput = UserInput <~ (lift .y Keyboard.wasd)
                       ~ (lift .y Keyboard.arrows)

{-
Combination of input and time signals
The let expession using foldps makes sure the Keyboard.space signal
 turns into a signal which turns true once when the Keyboard.Space
 signal goes from false to true. This can only be handled here because
 of the sampleOn function used to define sig.

input :: Signal Input
-}
input = sampleOn delta $ Input <~ delta ~ userInput

-- State signal
-- state :: Signal State
state = foldp stepGame defaultGame input

{- 
Lift the display of the State over a Signal of
the window dimensions and a Signal of the State.
-}
main = lift2 display Window.dimensions state
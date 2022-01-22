module Test.MySolutions where

import Prelude
import Math (sqrt, pi)
import Data.Int (rem)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = r * r * pi

-- point free madness!
leftoverCents :: Int -> Int
leftoverCents = flip rem $ 100

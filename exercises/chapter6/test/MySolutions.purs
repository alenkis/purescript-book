module Test.MySolutions where

import Data.Ord
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Point
  = Point { x :: Number, y :: Number }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> (show x) <> ", " <> (show y) <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = (show real) <> sign <> (show <<< abs $ imaginary) <> "i"
    where
    sign = if imaginary > 0.0 then "+" else "-"

derive instance eqComplex :: Eq Complex

instance semiringComplex :: Semiring Complex where
  add (Complex a1) (Complex a2) = Complex $ a1 + a2
  zero = Complex { real: 0.0, imaginary: 0.0 }
  mul (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) =
    Complex
      { real: r1 * r2 - (i1 * i2)
      , imaginary: r1 * i2 + i1 * r2
      }
  one = Complex { real: 1.0, imaginary: 0.0 }

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

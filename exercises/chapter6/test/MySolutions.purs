module Test.MySolutions where

import Data.Foldable
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Ord (abs, compare)
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

data NonEmpty a
  = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty el arr) = show ([ el ] <> arr)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [ e2 ] <> a2)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty el arr) = NonEmpty (f el) (map f arr)

-- derived solution
-- derive instance functorNonEmpty :: Functor NonEmpty
--
data Extended a
  = Infinite
  | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare (Finite a1) (Finite a2) = compare a1 a2
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f b (NonEmpty el arr) = foldr f b ([ el ] <> arr)
  foldl f b (NonEmpty el arr) = foldl f b ([ el ] <> arr)
  foldMap f (NonEmpty el arr) = foldMap f ([ el ] <> arr)

data OneMore f a
  = OneMore a (f a)

------------------------------
-- Units as function spaces --
------------------------------

newtype BaseQuantity a = BaseQuantity Double -- A Double with a phantom type to track the unit
type Quotient a b      = b -> a -- The unit a / b is represented by the function space b -> a
type Dimensionless     = BaseQuantity () -- A BaseQuantity for dimensionless quantities
type Inverse a         = Quotient Dimensionless a -- The inverse of a is 1 / a
type Product a b       = Quotient a (Inverse b) -- We can define a * b as a / (1 / b)
type Square a          = Product a a -- a^2 is a * a

-- This class represents types that can be converted to and from a Double.
class Quantity a where
  construct :: Double -> a
  destruct :: a -> Double

-- BaseQuantity is a Quantity since it just wraps a Double.
instance Quantity (BaseQuantity a) where
  construct = BaseQuantity
  destruct (BaseQuantity x) = x

-- Quotients of quantities are quantities as well. To construct a Quotient from a Double,
-- we define a function which destructs its argument, multiplies the result by the given
-- Double, and constructs a quantity of the return type (the numerator unit). To destruct
-- a Quotient, we first construct 1 in the denominator unit (the argument type), use the
-- quotient to convert it into the numerator unit, and destruct the result.
instance (Quantity a, Quantity b) => Quantity (a -> b) where
  construct x = \y -> construct (x * (destruct y))
  destruct x = destruct (x (construct 1))

-- a / (b / c) = c / (b / a)
quotientAxiom :: (Quantity a, Quantity b, Quantity c) =>
  Quotient a (Quotient b c) -> Quotient c (Quotient b a)
quotientAxiom = construct . destruct

-- We can add two quantities of the same unit.
infixl 6 .+.
(.+.) :: Quantity a => a -> a -> a
(.+.) x y = construct $ (destruct x) + (destruct y)

-- We can subtract two quantities of the same unit.
infixl 6 .-.
(.-.) :: Quantity a => a -> a -> a
(.-.) x y = construct $ (destruct x) - (destruct y)

-- We can multiply any two quantities.
infixl 7 .*.
(.*.) :: (Quantity a, Quantity b) => a -> b -> Product a b
(.*.) x y = \z -> construct $ destruct (z y) * destruct x

-- We can divide any two quantities.
infixl 7 ./.
(./.) :: (Quantity a, Quantity b) => a -> b -> Quotient a b
(./.) x y = \z -> construct $ (destruct z) * (destruct x) / (destruct y)

----------------
-- Base units --
----------------

data Meter
data Kilogram
data Second

-------------------
-- Type synonyms --
-------------------

type Length   = BaseQuantity Meter
type Mass     = BaseQuantity Kilogram
type Time     = BaseQuantity Second
type Area     = Square Length
type Velocity = Quotient Length Time

----------------------------------------
-- Example 1: Tracking units in types --
----------------------------------------

-- Let's calculate the area of a table.

tableWidth :: Length
tableWidth = construct 1.5

tableHeight :: Length
tableHeight = construct 2.5

tableArea :: Area
tableArea = tableWidth .*. tableHeight

-- Note: Calculations with quantities are type safe. Suppose we defined the mass of the table:
--
--       > tableMass :: Mass
--       > tableMass = construct 150
--
--       Then the following is a type error:
--
--       > tableArea :: Area
--       > tableArea = tableWidth .*. tableMass
--         Error: Couldn't match type ‘Kilogram’ with ‘Meter’
--         Expected type: Area
--         Actual type: Product Length Mass

----------------------------------------
-- Example 2: Quantities as functions --
----------------------------------------

-- Suppose we want to calculate how far a train will travel, given its velocity and
-- the duration of the trip.

trainVelocity :: Velocity
trainVelocity = construct 30

tripDuration :: Time
tripDuration = construct 5000

-- Here we demonstrate the correspondence between quantities and functions. Velocity is
-- a synonym for Length / Time, but it's also a function from Time to Length. Given a
-- Time, we can simply "apply" a Velocity to it to get a Length.
tripDistance :: Length
tripDistance = trainVelocity tripDuration

-- So multiplication of a / b by b is just function application.

-----------------------------------------------
-- Example 3: Manual proofs of unit equality --
-----------------------------------------------

-- Let's define a function that takes a Length and a Velocity and returns a Time.
-- First try:
--
-- > calculateDuration :: Length -> Velocity -> Time
-- > calculateDuration distance velocity = distance ./. velocity
--
--   Error: Couldn't match type ‘Velocity -> Length’ with ‘BaseQuantity Second’
--   Expected type: Time
--   Actual type: Quotient Length Velocity
--
-- Haskell doesn't know that Length / Velocity = Time. If this is indeed true, there will
-- be a way to manipulate the program (without destructing the quantity) to make it
-- type check by swapping arguments, using the identities like a / (b / c) = c / (b / a),
-- defining new functions, etc. This is similar to what one would do in a proof assistant
-- like Agda or Coq.
--
-- So we have:
--
--   distance ./. velocity :: Length / Velocity
--
-- Velocity is just a type synonym for Length / Time, so we actually have:
--
--   distance ./. velocity :: Length / (Length / Time)
--
-- We can apply the `quotientAxiom` to get:
--
--   quotientAxiom (distance ./. velocity) :: Time / (Length / Length)
--
-- Under the interpretation of units as function spaces, we have:
--
--   quotientAxiom (distance ./. velocity) :: (Length -> Length) -> Time
--
-- We can apply `id` to cancel the Lengths and get a Time. Putting it all together:
calculateDuration :: Length -> Velocity -> Time
calculateDuration distance velocity = quotientAxiom (distance ./. velocity) id

-- Now we can calculate how long the trip from Example 2 would take if the train traveled
-- at 8 m/s instead of 6 m/s.
fasterVelocity :: Velocity
fasterVelocity = construct 40

shorterDuration :: Time
shorterDuration = calculateDuration tripDistance fasterVelocity

------------------------
-- Program entrypoint --
------------------------

-- Here we just print the results from the examples. Result:
--   tableArea:       3.75 m^2
--   tripDistance:    150000.0 m
--   shorterDuration: 3750.0 s
main = do
  putStrLn $ "tableArea:       " ++ (show $ destruct $ tableArea) ++ " m^2"
  putStrLn $ "tripDistance:    " ++ (show $ destruct $ tripDistance) ++ " m"
  putStrLn $ "shorterDuration: " ++ (show $ destruct $ shorterDuration) ++ " s"

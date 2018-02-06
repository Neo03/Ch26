module MyRandom where
-- Neiborhood of infinity
import System.Random


bind :: (a -> StdGen -> (b,StdGen)) -> (StdGen -> (a,StdGen)) -> (StdGen -> (b,StdGen))
bind f x seed = let (x',seed') = x seed in f x' seed'
unit x g = (x,g)
lift f = unit . f

--  Conceptually we need: addDigit . (*10) . addDigit
--  But we know we need to thread the random seed through this code.
--  Firstly:

addDigit n g =
  let (a,g') = random g
  in (n + a `mod` 10,g')

-- *********************** in a suitable strict and impure language ************
{--
addDigit n =
  let a = random
  in n + a `mod` 10
--}

--  Now consider the operation to multiply by 10.
--  We can 'upgrade'this with "lift"

shift = lift (*10)

-- to compose that we must use bind:

test :: Integer -> StdGen ->  (Integer,StdGen)
test = bind addDigit . bind shift . addDigit

-- Now we create a seed

g = mkStdGen 23

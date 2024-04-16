import Iso
-- * HOMEWORK 1B
--
-- 1) Add a new primitive constructor
--
--      translate :: Region -> Point -> Region 
--
--    that translates the origin to the given point
--    for the given figure.
--
-- 2) Add a new interpreter function `serialize` that generates 
--    a textual representation of the region. The textual
--    representation should be valid Haskell source code
--    that recreates the region when pasted into a program.
--
--      e.g., > serialize (annulus 1 2)
--            "outside (circle 1) /\ circle 2"

-- * Common Code

type Radius = Float
type Point = (Float, Float)
type Side = Float

-- * Deep Embedding

-- ** Sorts

data Region -- Abstract Syntax Tree
 = Circle Radius
 | Outside Region
 | (:/\) Region Region -- operator constructor names start with a colon
 | Square Side -- new feature
 | Translate Region Point

-- ** Primitive Constructors

circle :: Radius -> Region
circle = Circle
outside :: Region -> Region
outside = Outside
(/\) :: Region -> Region -> Region
(/\) = (:/\)
square :: Side -> Region
square = Square

translate :: Region -> Point -> Region
translate = Translate

-- ** Derived Constructors

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2

-- ** Interpretation Functions

inRegion :: Region -> (Point -> Bool)
{- direct definition
inRegion (Circle r)  = \(x,y) -> x^2 + y^2 <= r^2
inRegion (Outside r) = \p -> not (inRegion r p)
inRegion (r1 :/\ r2) = \p -> inRegion r1 p && inRegion r2 p
inRegion (Translate r (x0,y0)) = \(x,y) -> inRegion r (x - x0, y - y0)
-}
  -- definition in terms of generic structural recursion scheme
inRegion = foldRegion c o i sq t where
  c r (x,y) = x^2 + y^2 <= r^2
  o r p = not (r p)
  i r1 r2 p = r1 p && r2 p
  sq s (x,y) = abs x <= s/2 && abs y <= s/2
  t r (x0,y0) (x,y) = r (x - x0, y - y0)

serialize :: Region -> String
serialize = foldRegion c o i sq t where
  c r = "circle " ++ show r
  o r = "outside (" ++ r ++ ")"
  i r1 r2 = r1 ++ " /\\ " ++ r2
  sq s = "square " ++ show s 
  t r p = "translate (" ++ r ++ ") " ++ show p


 -- structural recursion scheme
foldRegion :: (Radius -> a) -> (a -> a) -> (a -> a -> a) -> (Side -> a) -> (a -> Point -> a) -> Region -> a
foldRegion c o i sq t (Circle r)  = c r
foldRegion c o i sq t (Outside r) = o (foldRegion c o i sq t r)
foldRegion c o i sq t (r1 :/\ r2) = (foldRegion c o i sq t r1) `i` (foldRegion c o i sq t r2)
foldRegion c o i sq t (Square s)  = sq s
foldRegion c o i sq t (Translate r p) = t (foldRegion c o i sq t r) p

data RegionAlgebra a =
  RAlg {
    ra_c  :: Radius -> a,
    ra_o  :: a -> a,
    ra_i  :: a -> a -> a,
    ra_sq :: Side -> a,
    ra_t  :: a -> Point -> a 
  }

foldRegion' :: RegionAlgebra a -> Region -> a
foldRegion' alg (Circle r)       = ra_c alg r
foldRegion' alg (Outside r)      = ra_o alg (foldRegion' alg r)
foldRegion' alg (r1 :/\ r2)      = ra_i alg (foldRegion' alg r1) (foldRegion' alg r2)
foldRegion' alg (Square s)       = ra_sq alg s
foldRegion' alg (Translate r p)  = ra_t alg (foldRegion' alg r) p

foldRegion'' :: Region -> (RegionAlgebra a -> a)
foldRegion'' = to swapParams foldRegion'

-- * HOMEWORK 2B

-- create a shallow embedding based on foldRegion''

-- A specific regionAlgebra implementation to test the generic shallow embedding

inRegionAlgebra :: RegionAlgebra (Point -> Bool)
inRegionAlgebra = RAlg {
  ra_c  = \r (x,y) -> x^2 + y^2 <= r^2,
  ra_o  = \r p -> not (r p),
  ra_i  = \r1 r2 p -> r1 p && r2 p,
  ra_sq = \s (x,y) -> abs x <= s/2 && abs y <= s/2,
  ra_t  = \r (x0,y0) (x,y) -> r (x - x0, y - y0)
}

-- The shallow embedding

circleShallow :: Radius -> (RegionAlgebra a -> a)
circleShallow r alg = ra_c alg r

outsideShallow :: a -> (RegionAlgebra a -> a)
outsideShallow r alg = ra_o alg r

insideShallow :: a -> a -> (RegionAlgebra a -> a)
insideShallow r1 r2 alg = ra_i alg r1 r2

squareShallow :: Side -> (RegionAlgebra a -> a)
squareShallow s alg = ra_sq alg s

translateShallow :: a -> Point -> (RegionAlgebra a -> a)
translateShallow r p0 alg = ra_t alg r p0

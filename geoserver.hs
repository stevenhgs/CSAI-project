type Radius = Float
type Side = Float
type Point = (Float, Float, Float)

data Region = -- Abstract syntax tree
    Sphere Radius
    | Cube Side
    | Translate Region Point
    | Outside Region 
    | Intersection Region Region
    | Union Region Region

data RegionAlgebra a =
  RAlg {
    ra_sphere  :: Radius -> a,
    ra_cube :: Side -> a,
    ra_translate  :: a -> Point -> a,
    ra_outside  :: a -> a,
    ra_intersection  :: a -> a -> a,
    ra_union :: a -> a -> a
  }

foldRegion :: RegionAlgebra a -> Region -> a
foldRegion alg (Sphere r)       = ra_sphere alg r
foldRegion alg (Cube s)         = ra_cube alg s
foldRegion alg (Translate r p)  = ra_translate alg (foldRegion alg r) p
foldRegion alg (Outside r)      = ra_outside alg (foldRegion alg r)
foldRegion alg (Intersection r1 r2) = ra_intersection alg (foldRegion alg r1) (foldRegion alg r2)
foldRegion alg (Union r1 r2) = ra_union alg (foldRegion alg r1) (foldRegion alg r2)


-- inRegion interpretation
inRegion :: Region -> (Point -> Bool)
inRegion = undefined

-- serialize interpretation
serialize :: Region -> String
serialize = undefined
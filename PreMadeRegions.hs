module PreMadeRegions where
import Region

sphere1 = SphereGS 0.6
cube1 = CubeGS 0.5
translatedSphere1 = TranslateGS sphere1 (0.5, 0.5, 0.5)
translatedCube1 = TranslateGS cube1 (0, 0.25, 0)
outsideCube1 = OutsideGS cube1
unionTranslatedSphere1TranslatedCube1 = UnionGS translatedSphere1 translatedCube1
interSectionTranslatedCube1TranslatedSphere1 = IntersectionGS translatedCube1 translatedSphere1

translatedSphere2 = TranslateGS sphere1 (0.2, 0.2, 0)
translatedSphere3 = TranslateGS sphere1 (0.2, -0.2, 0)
translatedSphere4 = TranslateGS sphere1 (-0.2, -0.2, 0)
translatedSphere5 = TranslateGS sphere1 (-0.2, 0.2, 0)

union1 = UnionGS translatedSphere2 translatedSphere3
union2 = UnionGS translatedSphere4 translatedSphere5
unionUlt = UnionGS union1 union2

intersection1 = IntersectionGS translatedSphere2 translatedSphere3
intersection2 = IntersectionGS translatedSphere4 translatedSphere5
intersectionUlt = IntersectionGS intersection1 intersection2
unionOfIntersections = UnionGS intersection1 intersection2

intersectionOfCubeANdIntersection = IntersectionGS translatedCube1 intersection1
intersectionOfCubeANdUnionOfIntersections = IntersectionGS translatedCube1 unionOfIntersections


outsideCube = OutsideGS cube1
translatedOutsideCube = TranslateGS outsideCube (0, 0.25, 0)
outsideTranslatedOutsideCube = OutsideGS translatedOutsideCube

-- for this one outside is only used to get StencilBuffer
intersectionOutsideAndUnion = IntersectionGS translatedOutsideCube union2
-- for this one outside is used to get StencilBuffer and to draw
intersectionUnionAndOutside = IntersectionGS union2 translatedOutsideCube
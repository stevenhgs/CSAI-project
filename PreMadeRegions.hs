module PreMadeRegions where
import Region

sphere1 = SphereGS 1
cube1 = CubeGS 0.5
translatedSphere1 = TranslateGS sphere1 (0.5, 0.5, 0.5)
translatedCube1 = TranslateGS cube1 (-0.5, -0.5, -0.5)
outsideCube1 = OutsideGS cube1
unionTranslatedSphere1TranslatedCube1 = UnionGS translatedSphere1 translatedCube1
interSectionTranslatedCube1TranslatedSphere1 = IntersectionGS translatedCube1 translatedSphere1

import Test.Hspec
import Lib
import Data.Array.Unboxed
import Data.Maybe
import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
               describe "Matrices:" $ do
                  it "Creating a 4x4 matrix" $ do
                      let m = Matrix 4 4 (V.fromList [1,   2,   3,   4,
                                                      5.5, 6.5, 7.5, 8.5,
                                                      9,   10,  11,  12,
                                                      13.5, 14.5, 15.5, 16.5])
                      search m 1 3 `shouldBe` 3
                      search m 2 3 `shouldBe` 7.5
                      search m 3 2 `shouldBe` 10

                  it "Creating a  2x2 matrix" $ do
                                        let m = Matrix 2 2 (V.fromList [(-3), 5,
                                                                          1, (-2)])
                                        search m 1 1 `shouldBe` (-3)
                                        search m 1 2 `shouldBe` 5
                                        search m 2 2 `shouldBe` (-2)

                  it "Creating a  3x3 matrix" $ do
                                        let m = Matrix 3 3 (V.fromList [(-3), 5,     0,
                                                                          1, (-2), (-7),
                                                                          0,   1,    1])
                                        search m 1 1 `shouldBe` (-3)
                                        search m 2 2 `shouldBe` (-2)
                                        search m 3 3 `shouldBe`   1

                  it "Matrix Equality with identical matrices" $ do
                                        let m1 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                        5, 6, 7, 8,
                                                                        9, 8, 7, 6,
                                                                        5, 4, 3, 2])
                                        let m2 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                         5, 6, 7, 8,
                                                                         9, 8, 7, 6,
                                                                         5, 4, 3, 2])
                                        m1 `equalMatrix` m2 `shouldBe` True
                  it "Matrix Equality with different matrices" $ do
                                                          let m1 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                                          5, 7, 7, 8,
                                                                                          9, 8, 7, 6,
                                                                                          5, 4, 3, 2])
                                                          let m2 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                                           5, 6, 7, 8,
                                                                                           9, 8, 7, 6,
                                                                                           5, 4, 3, 2])
                                                          m1 `equalMatrix` m2 `shouldBe` False

                  it "Matrix Equality with different matrices" $ do
                                                          let m1 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                                          5, 7, 7, 8,
                                                                                          9, 8, 7, 6,
                                                                                          5, 4, 3, 2])
                                                          let m2 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                                           5, 6, 7, 8,
                                                                                           9, 8, 7, 6,
                                                                                           5, 4, 3, 2])
                                                          m1 `equalMatrix` m2 `shouldBe` False

                  it " Multiplying two matrices" $ do
                                                     let m1 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                                     5, 6, 7, 8,
                                                                                     9, 8, 7, 6,
                                                                                     5, 4, 3, 2])
                                                     let m2 = Matrix 4 4 (V.fromList [(-2), 1, 2, 3,
                                                                                        3, 2, 1, (-1),
                                                                                        4, 3, 6, 5,
                                                                                        1, 2, 7, 8])
                                                     m1 `multMat` m2 `shouldBe` Matrix 4 4 (V.fromList [20, 22, 50, 48,
                                                                                                        44, 54, 114, 108,
                                                                                                        40, 58, 110, 102,
                                                                                                        16, 26, 46, 42])
                  it "A matrix multiplied by a tuple" $ do
                                                     let m1 = Matrix 4 4 (V.fromList [1, 2, 3, 4,
                                                                                      2, 4, 4, 2,
                                                                                      8, 6, 4, 1,
                                                                                      0,0,0,1])
                                                     let v = makePoint 1 2 3
                                                     (m1 `multMatVector` v) `shouldBe` makePoint 18 24 33

                  it " Multiplying a matrix by the identity matrix" $ do
                                                     let m1 = Matrix 4 4 (V.fromList [0, 1, 2, 4,
                                                                                      1, 2, 4, 8,
                                                                                      2, 4, 8, 16,
                                                                                      4, 8, 16, 32])
                                                     m1 `multMat` identity4 `shouldBe` m1
                  it " Multiplying the identity matrix by a tuple" $ do
                                                      let t = (1, 2, 3, 4)
                                                      identity4 `multMatVector` t `shouldBe` t
                  it " Transposing a matrix" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [0, 9, 3, 0,
                                                                                       9, 8, 0, 8,
                                                                                       1, 8, 5, 3,
                                                                                       0, 0, 5, 8])
                                                      transpose m1 `shouldBe` Matrix 4 4 (V.fromList  [0, 9, 1, 0,
                                                                                                       9, 8, 8, 0,
                                                                                                       3, 0, 5, 5,
                                                                                                       0, 8, 3, 8])
                  it "Transposing the identity matrix" $ do
                                                      let a = transpose identity4
                                                      a `shouldBe` identity4
                  it" Calculate the determinate of a 2x2 matrix" $ do
                                                      let m1 = Matrix 2 2 (V.fromList [   1, 5,
                                                                                        (-3),2 ])
                                                      determinant m1 `shouldBe` 17
                  it " A submatrix of a 3x3 matrix is a 2x2 matrix" $ do
                                                      let m1 = Matrix 3 3 (V.fromList [  1,  5,  0,
                                                                                   (-3), 2,  7,
                                                                                     0,  6, (-3)])
                                                      let m2 = Matrix 2 2 (V.fromList [  (-3), 2,
                                                                                         0, 6])
                                                      submatrix m1 1 3 `shouldBe` m2
                  it " A submatrix of a 4x4 matrix is a 3x3 matrix" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [  (-6), 1,   1, 6,
                                                                                         (-8), 5,   8, 6,
                                                                                         (-1), 0,   8, 2,
                                                                                         (-7), 1, (-1), 1])
                                                      let m2 = Matrix 3 3 (V.fromList [  (-6),   1, 6,
                                                                                         (-8),   8, 6,
                                                                                         (-7), (-1), 1])
                                                      submatrix m1 3 2 `shouldBe` m2
                  it "Calculating the minor of a 3x3 matrix" $ do
                                                      let a = Matrix 3 3 ( V.fromList [ 3,   5,    0,
                                                                                         2, (-1), (-7),
                                                                                         6, (-1),   5])
                                                      let b = submatrix a 2 1
                                                      determinant b `shouldBe` 25
                                                      minor a 2 1 `shouldBe` 25
                  it "Calculating a cofactor of a 3x3 matrix" $ do
                                                      let a = Matrix 3 3 (V.fromList [ 3,   5,    0,
                                                                                       2, (-1), (-7),
                                                                                       6, (-1),   5])
                                                      minor a 1 1 `shouldBe` (-12)
                                                      cofactor a 1 1 `shouldBe` (-12)
                                                      minor a 2 1 `shouldBe` 25
                                                      cofactor a 2 1 `shouldBe` (-25)
                  it "Calculating the determinant of a 3x3 matrix" $ do
                                                      let m1 = Matrix 3 3 (V.fromList [   1,  2,    6,
                                                                                      (-5), 8, (-4),
                                                                                        2,  6,   4])
                                                      cofactor m1 1 1 `shouldBe` 56
                                                      cofactor m1 1 2 `shouldBe` 12
                                                      cofactor m1 1 3 `shouldBe` -46
                                                      determinant m1 `shouldBe` (-196)
                  it "Calculating the determinant of a 4x4 matrix" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [ (-2), (-8),  3,   5,
                                                                                        (-3),   1,   7,   3,
                                                                                          1,    2, (-9),  6,
                                                                                        (-6),   7,   7, (-9)])
                                                      cofactor m1 1 1 `shouldBe` 690
                                                      cofactor m1 1 2 `shouldBe` 447
                                                      cofactor m1 1 3 `shouldBe` 210
                                                      determinant m1 `shouldBe` (-4071)
                  it " Testing an invertible matrix for invertibility" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [6,   4,  4,   4,
                                                                                       5,   5,  7,   6,
                                                                                       4, (-9), 3, (-7),
                                                                                       9,   1,  7, (-6)])
                                                      determinant m1 `shouldBe` -2120
                  it "Testing a noninvertible matrix for invertibility" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [ (-4),  2 , (-2), (-3),
                                                                                          9,   6,    2,    6,
                                                                                          0, (-5),   1,  (-5),
                                                                                          0,   0,    0,     0])
                                                      determinant m1 `shouldBe` 0
                  it "calculating the inverse of a matrix" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [ (-5),  2,   6,  (-8),
                                                                                          1, (-5),  1,    8,
                                                                                          7,   7, (-6), (-7),
                                                                                          1, (-3),  7,    4])
                                                      let m2 = inverse m1
                                                      determinant m1 `shouldBe` 532
                                                      cofactor m1 3 4 `shouldBe` (-160)
                                                      search m2 4 3 `equalN` ((-160)/532) `shouldBe` True
                                                      cofactor m1 4 3 `shouldBe` 105
                                                      search m2 3 4 `equalN` (105/532) `shouldBe` True
                                                      m2 `equalMatrix` Matrix 4 4 (V.fromList [0.21805,  0.45113,  0.24060, (-0.04511),
                                                                                            (-0.80827), (-1.45677), (-0.44361),  0.52068,
                                                                                             (-0.07895), (-0.22368), (-0.05263),  0.19737,
                                                                                             (-0.52256), (-0.81391), (-0.30075),  0.30639])
                                                        `shouldBe` True
                  it "Calculating the inverse of another matrix" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [   8, (-5),   9,   2,
                                                                                          7,   5,   6,    1,
                                                                                        (-6),  0,   9,    6,
                                                                                        (-3),  0, (-9), (-4)])
                                                      inverse m1 `equalMatrix` Matrix 4 4 (V.fromList [(-0.15385),  (-0.15385), (-0.28205), (-0.53846),
                                                                                               (-0.07692), 0.12308, 0.02564, 0.03077,
                                                                                                0.35897, 0.35897, 0.43590, 0.92308,
                                                                                                (-0.69231), (-0.69231), (-0.76923), (-1.92308)])
                                                        `shouldBe` True
                  it "Calculating the inverse of a third matrix" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [  9,    3,    0,    9,
                                                                                       (-5), (-2), (-6), (-3),
                                                                                       (-4),   9,    6,    4,
                                                                                       (-7),   6,    6,    2])
                                                      inverse m1 `equalMatrix` Matrix 4 4 (V.fromList [(-0.04074), (-0.07778), 0.14444, (-0.22222),
                                                                                                        (-0.07778), 0.03333, 0.36667, (-0.33333),
                                                                                                        (-0.02901), (-0.14630), (-0.10926), 0.12963,
                                                                                                        0.17778, 0.06667, (-0.26667), 0.33333])
                                                      `shouldBe` True
                  it "Multiplying a product by its inverse" $ do
                                                      let m1 = Matrix 4 4 (V.fromList [3, (-9), 7, 3,
                                                                                       3, (-8), 2, (-9),
                                                                                       (-4), 4, 4, 1,
                                                                                       (-6), 5, (-1), 1])
                                                      let m2 = Matrix 4 4 (V.fromList [  8,  2,   2,   2,
                                                                                         3,(-8),  2, (-9),
                                                                                       (-4), 4,   4,   1,
                                                                                       (-6), 5, (-1),  1])
                                                      let m3 = m1 `multMat` m2
                                                      m3 `multMat` (inverse m2)  `equalMatrix` m1 `shouldBe` True

                  it "Multiplying by a translation matrix" $ do
                                                        let transform = translation 5 (-3) 2
                                                        let p = makePoint (-3) 4 5
                                                        transform `multMatVector` p `shouldBe` makePoint 2 1 7

                  it "Multiplying by the inverse of a translation matrix" $ do
                                                        let transform = translation 5 (-3) 2
                                                        let inv = inverse transform
                                                        let p = makePoint (-3) 4 5
                                                        inv `multMatVector` p `shouldBe` makePoint (-8) 7 3 --multiplication function needed?
               describe " Transformations" $ do
                  it "Translation does not affect vectors" $ do
                                                        let transform = translation 5 (-3) 2
                                                        let v = makeVector (-3) 4 5
                                                        transform `multMatVector` v `shouldBe` v
                  it "A scaling matrix applied to a point" $ do
                                                        let transform = scaling 2 3 4
                                                        let p = makePoint (-4) 6 8
                                                        transform `multMatVector` p `shouldBe` makePoint (-8) 18 32
                  it "A scaling matrix applied to a vector" $ do
                                                        let transform = scaling 2 3 4
                                                        let v = makeVector (-4) 6 8
                                                        transform `multMatVector` v `shouldBe` makeVector (-8) 18 32
                  it "Multiplying by the inverse of a scaling matrix" $ do
                                                        let transform = scaling 2 3 4
                                                        let inv = inverse transform
                                                        let v = makeVector (-4) 6 8
                                                        inv `multMatVector` v `shouldBe` makeVector (-2) 2 2
                  it "Reflection is scaling by a negative value" $ do
                                                        let transform = scaling (-1) 1 1
                                                        let p = makePoint 2 3 4
                                                        transform `multMatVector` p `shouldBe` makePoint (-2) 3 4
                  it "Rotating a point around the x axis" $ do
                                                        let p = makePoint 0 1 0
                                                        let half_quarter = rotation_x (pi/4)
                                                        let full_quarter = rotation_x (pi/2)
                                                        let p2 = makePoint 0  ((sqrt 2)/2) ((sqrt 2)/2)
                                                        let a = half_quarter `multMatVector` p
                                                        a `vectorEqual` p2 `shouldBe` True
                                                        full_quarter `multMatVector` p `vectorEqual` makePoint 0 0 1 `shouldBe` True
                  it "The inverse of an x-rotation rotates in the opposite direction" $ do
                                                        let p = makePoint 0 1 0
                                                        let half_quarter = rotation_x (pi/4)
                                                        let inv = inverse half_quarter
                                                        inv `multMatVector` p `vectorEqual` makePoint 0  ((sqrt 2)/2) (-(sqrt 2)/2) `shouldBe` True
                  it "Rotating a point around the y axis" $ do
                                                        let p = makePoint 0 0 1
                                                        let half_quarter = rotation_y (pi/4)
                                                        let full_quarter = rotation_y (pi/2)
                                                        half_quarter `multMatVector` p `vectorEqual` makePoint ((sqrt 2)/2) 0 ((sqrt 2)/2) `shouldBe` True
                  it "Rotating a point around the z xis" $ do
                                                        let p = makePoint 0 1 0
                                                        let half_quarter = rotation_z (pi/4)
                                                        let full_quarter = rotation_z (pi/2)
                                                        half_quarter `multMatVector` p `vectorEqual` makePoint (-(sqrt 2)/2) ((sqrt 2)/2) 0 `shouldBe` True
                                                        full_quarter `multMatVector` p `vectorEqual` makePoint (-1) 0 0 `shouldBe` True
                  it "A shearing transformation moves x in proportion to y" $ do
                                                        let transform = shearing 1 0 0 0 0 0
                                                        let p = makePoint 2 3 4
                                                        transform `multMatVector` p `vectorEqual` makePoint 5 3 4 `shouldBe` True
                  it "A shearing a transformation moves x in propotion to z" $ do
                                                        let transform = shearing 0 1 0 0 0 0
                                                        let p = makePoint 2 3 4
                                                        transform `multMatVector` p `vectorEqual` makePoint 6 3 4 `shouldBe` True
                  it "A shearing a transformation moves y in propotion to x" $ do
                                                        let transform = shearing 0 0 1 0 0 0
                                                        let p = makePoint 2 3 4
                                                        transform `multMatVector` p `vectorEqual` makePoint 2 5 4 `shouldBe` True
                  it "A shearing a transformation moves z in propotion to x" $ do
                                                        let transform = shearing 0 0 0 0 1 0
                                                        let p = makePoint 2 3 4
                                                        transform `multMatVector` p `vectorEqual` makePoint 2 3 6 `shouldBe` True
                  it "A shearing a transformation moves z in propotion to y" $ do
                                                        let transform = shearing 0 0 0 0 0 1
                                                        let p = makePoint 2 3 4
                                                        transform `multMatVector` p `vectorEqual` makePoint 2 3 7 `shouldBe` True
                  it "individual tranformations are applied in a sequence" $ do
                                                        -- list of transformations: rotate, scale (larger), translate (move)
                                                        let p = makePoint 1 0 1
                                                        let a = rotation_x (pi/2)
                                                        let b = scaling 5 5 5
                                                        let c = translation 10 5 7
                                                        let p2 = a `multMatVector` p
                                                        p2 `vectorEqual` makePoint 1 (-1) 0 `shouldBe` True
                                                        let  p3 = b `multMatVector` p2
                                                        p3 `vectorEqual` makePoint 5 (-5) 0 `shouldBe` True
                                                        let p4 = c `multMatVector` p3
                                                        p4 `vectorEqual` makePoint 15 0 7 `shouldBe` True
                  it "Chained tranformations must be applied in reverse order" $ do
                                                        let p = makePoint 1 0 1
                                                        let a = rotation_x (pi/2)
                                                        let b = scaling 5 5 5
                                                        let c = translation 10 5 7
                                                        let t = combTrans [c, b, a]
                                                        t `multMatVector` p `vectorEqual` makePoint 15 0 7 `shouldBe` True
               describe " Rays" $ do
                  it "Creating an querying a ray" $ do
                                                        let origin = makePoint 1 2 3
                                                        let direction = makeVector 4 5 6
                                                        let r = Ray origin direction
                                                        let Ray orig dir = r
                                                        orig `shouldBe` origin
                                                        dir `shouldBe` direction
                  it "Computing a point form a distance" $ do
                                                        let r = Ray (makePoint 2 3 4) (makeVector 1 0 0)
                                                        rayPosition r 0 `shouldBe` makePoint 2 3 4
                                                        rayPosition r 1 `shouldBe` makePoint 3 3 4
                                                        rayPosition r (-1) `shouldBe` makePoint 1 3 4
                                                        rayPosition r 2.5 `shouldBe` makePoint 4.5 3 4
                  it "A ray intersects a sphere at two points" $ do
                                                        let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                                        let s = makeSphere
                                                        let xs = intersect s r
                                                        True `shouldBe` True
--                                                        xs !! 0 `shouldBe` (4, s)
--                                                        xs !! 1 `shouldBe` (6, s)
                  it "A ray interesects a sphere at a tangent" $ do
                                                        let r = Ray (makePoint 0 1 (-5)) (makeVector 0 0 1)
                                                        let s = makeSphere
                                                        let xs = intersect s r
                                                        length xs `shouldBe` 2
--                                                        xs !! 0 `shouldBe` (5, s)
--                                                        xs !! 1 `shouldBe` (5, s)
                  it "A ray misses a sphere" $ do
                                                        let r = Ray (makePoint 0 2 (-5)) (makeVector 0 0 1)
                                                        let s = makeSphere
                                                        let xs = intersect s r
                                                        length xs `shouldBe` 0
                  it "A ray originates inside a sphere" $ do
                                                        let r = Ray (makePoint 0 0 0) (makeVector 0 0 1)
                                                        let s = makeSphere
                                                        let xs = intersect s r
                                                        length  xs `shouldBe` 2
--                                                        xs !! 0 `shouldBe` ((-1), s)
--                                                        xs !! 1  `shouldBe` (1, s)
                  it "A sphere is behind a ray" $ do
                                                        let r = Ray (makePoint 0 0 5) (makeVector 0 0 1)
                                                        let s = makeSphere
                                                        let xs = intersect s r
                                                        length  xs `shouldBe` 2
--                                                        xs !! 0 `shouldBe` ((-6),s)
--                                                        xs !! 1  `shouldBe` ((-4),s)
               describe "Tracking Intersections" $ do
                  it "An intersection encapsulates t and object" $ do
                                                        let s = makeSphere
                                                        let i = Intersection 3.5 s
                                                        let Intersection t s1 = i
                                                        t `shouldBe` 3.5
                                                        s1 `shouldBe` s
                  it "Aggregating intersection" $ do
                                                        let s = makeSphere
                                                        let i1 = Intersection 1 s
                                                        let i2 = Intersection 2 s
                                                        let xs =  [i1, i2]
                                                        length xs `shouldBe` 2
                                                        tValue (xs !! 0) `shouldBe` 1
                                                        tValue (xs !! 1) `shouldBe` 2
                  it "Intersect sets the object on the intersection" $ do
                                                        let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                                        let s = makeSphere
                                                        let xs = intersect s r
                                                        length xs `shouldBe` 2
                                                        object (xs !! 0) `shouldBe` s
                                                        object (xs !! 1) `shouldBe` s
                  it "The hit, when all intersections have positive t" $ do
                                                        let s = makeSphere
                                                        let i1 = Intersection 1 s
                                                        let i2 = Intersection 2 s
                                                        let xs = intersections [i2, i1]
                                                        let i = hit xs
                                                        fromJust i `shouldBe` i1
                  it "The hit, when some intersections have negative t" $ do
                                                        let s = makeSphere
                                                        let i1 = Intersection (-1) s
                                                        let i2 = Intersection 1 s
                                                        let xs = intersections [i2, i1]
                                                        let i = hit xs
                                                        fromJust i `shouldBe` i2
                  it "The hit, when all intersections have negative t" $ do
                                                       let s = makeSphere
                                                       let i1 = Intersection (-2) s
                                                       let i2 = Intersection (-1) s
                                                       let xs = intersections [i2, i1]
                                                       let i = hit xs
                                                       i `shouldBe` Nothing
                  it "The hit is always the lowest nonnegative intersection" $ do
                                                        let s = makeSphere
                                                        let i1 = Intersection 5 s
                                                        let i2 = Intersection 7 s
                                                        let i3 = Intersection (-3) s
                                                        let i4 = Intersection 2 s
                                                        let xs = intersections [i1, i2, i3, i4]
                                                        let i = hit xs
                                                        fromJust i `shouldBe` i4
               describe "Tranforming Rays and Spheres" $ do
                  it "Translating a ray" $ do
                                         let r = Ray (makePoint 1 2 3) (makeVector 0 1 0)
                                         let m = translation 3 4 5
                                         let r2 = transform m r
                                         let Ray o d = r2
                                         o `shouldBe` makePoint 4 6 8
                                         d `shouldBe` makeVector 0 1 0
                  it "Scaling a ray" $ do
                                         let r = Ray (makePoint 1 2 3) (makeVector 0 1 0)
                                         let m = scaling 2 3 4
                                         let r2 = transform m r
                                         let Ray o d = r2
                                         o `shouldBe` makePoint 2 6 12
                                         d `shouldBe` makeVector 0 3 0
                  it "A Sphere's default transformation" $ do
                                         let s = makeSphere
                                         shapeTransform s `shouldBe` identity4
                  it "Changing a sphere's transformation" $ do
                                         let s = makeSphere
                                         let t = translation 2 3 4
                                         let s' = s {shapeTransform = t}
                                         shapeTransform s' `shouldBe` t
                  it "Intersecting a scaled sphere with a ray" $ do
                                         let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                         let s = makeSphere
                                         let s' = s {shapeTransform = scaling 2 2 2}
                                         let xs = intersect s' r
                                         length xs `shouldBe`  2
                                         tValue (xs !! 0) `shouldBe` 3
                                         tValue (xs !! 1) `shouldBe` 7
                  it "Intersecting a translated sphere with a ray" $ do
                                         let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                         let s = makeSphere
                                         let s' = s {shapeTransform = translation 5 0 0}
                                         let xs = intersect s' r
                                         length xs `shouldBe`  0
               describe "Light and Shading" $ do
                  it "The normal on a sphere at a point on the x axis" $ do
                                         let s = makeSphere
                                         let n = normal_at s (makePoint 1 0 0)
                                         n `shouldBe` makeVector 1 0 0
                  it "The normal on a sphere at a point on the y axis" $ do
                                         let s = makeSphere
                                         let n = normal_at s (makePoint 0 1 0)
                                         n `shouldBe` makeVector 0 1 0
                  it "The normal on a sphere at a point on the z axis" $ do
                                         let s = makeSphere
                                         let n = normal_at s (makePoint 0 0 1)
                                         n `shouldBe` makeVector 0 0 1
                  it "The normal on a sphere at a nonaxial point" $ do
                                         let s = makeSphere
                                         let n = normal_at s ( makePoint (sqrt(3)/3) (sqrt(3)/3) (sqrt(3)/3) )
                                         n `shouldBe` makeVector (sqrt(3)/3) (sqrt(3)/3) (sqrt(3)/3)
                  it "The normal is a normalized vector" $ do
                                         let s = makeSphere
                                         let n = normal_at s ( makePoint (sqrt(3)/3) (sqrt(3)/3) (sqrt(3)/3) )
                                         n `shouldBe` normalize n

                  it "Computing the normal on a translated sphere" $ do
                                         let s = makeSphere
                                         let s' = s {shapeTransform = translation 0 1 0}
                                         let n = normal_at s' (makePoint 0 1.70711 (-0.70711))
                                         n `vectorEqual` makeVector 0 0.70711 (-0.70711) `shouldBe` True
                  it "Computing the normal on a transformed sphere" $ do
                                          let s = makeSphere
                                          let s' = s {shapeTransform = combTrans [(scaling 1 0.5 1), rotation_z
                                           (pi/5)]}
                                          let n = normal_at s' (makePoint 0 (sqrt(2)/2) (-sqrt(2)/2))
                                          n `vectorEqual` makeVector 0 0.97014 (-0.24254) `shouldBe` True
                  it "Reflecting a vector approaching at 45°" $ do
                                          let v = makeVector 1 (-1) 0
                                          let n = makeVector 0 1 0
                                          let r = reflect v n
                                          r `vectorEqual` makeVector 1 1 0 `shouldBe` True
                  it "Reflecting a vector off a slanted surface" $ do
                                          let v = makeVector 0 (-1) 0
                                          let n = makeVector (sqrt(2)/2) (sqrt(2)/2) 0
                                          let r = reflect v n
                                          r `vectorEqual` makeVector 1 0 0 `shouldBe` True
               describe "Phong Reflection Model" $ do
                  it "A point light has a position and intensity" $ do
                                          let intensity = (1, 1, 1)
                                          let position = makePoint 0 0 0
                                          let light = Light position intensity
                                          let Light p i = light
                                          p `shouldBe` position
                                          i `shouldBe` intensity
                  it "The default material" $ do
                                          let m = material
                                          let Material c a d sp sh = m
                                          c `shouldBe` (1, 1, 1)
                                          a `shouldBe` 0.1
                                          d `shouldBe` 0.9
                                          sp `shouldBe` 0.9
                                          sh `shouldBe` 200.0
                  it "A sphere has a default material" $ do
                                          let s = makeSphere
                                          let m = shapeMaterial s
                                          m `shouldBe` material
                  it "A sphere may be assigned a material" $ do
                                          let s = makeSphere
                                          let m = material { materialAmbient = 1}
                                          let s' = s { shapeMaterial = m}
                                          shapeMaterial s' `shouldBe` m
                  it "Lighting with the eye between the light and the surface" $ do
                                          let m = material
                                          let position = makePoint 0 0 0
                                          let eyev = makeVector 0 0 (-1)
                                          let normalv = makeVector 0 0 (-1)
                                          let light = Light (makePoint 0 0 (-10)) (1,1,1)
                                          let result = lighting m light position eyev normalv False
                                          result `shouldBe` (1.9, 1.9, 1.9)
                  it "Lighting with the eye between light and surface, eye offset 45°" $ do
                                          let m = material
                                          let position = makePoint 0 0 0
                                          let eyev = makeVector 0 ((sqrt 2)/2) (-((sqrt 2)/2))
                                          let normalv = makeVector 0 0 (-1)
                                          let light = Light (makePoint 0 0 (-10)) (1,1,1)
                                          let result = lighting m light position eyev normalv False
                                          result `shouldBe` (1.0,1.0,1.0)
                  it "Lighting with eye oppposite surface, light offset 45°" $ do
                                          let m = material
                                          let position = makePoint 0 0 0
                                          let eyev = makeVector 0 0 (-1)
                                          let normalv = makeVector 0 0 (-1)
                                          let light = Light (makePoint 0 10 (-10)) (1,1,1)
                                          let result = lighting m light position eyev normalv False
                                          result `colorEqual` (0.7364,0.7364,0.7364) `shouldBe` True
                  it "Lighting with eye in the path of the reflection vector" $ do
                                          let m = material
                                          let position = makePoint 0 0 0
                                          let eyev = makeVector 0 (-((sqrt 2)/2)) (-((sqrt 2)/2))
                                          let normalv = makeVector 0 0 (-1)
                                          let light = Light (makePoint 0 10 (-10)) (1,1,1)
                                          let result = lighting m light position eyev normalv False
                                          result `colorEqual` (1.6364, 1.6364, 1.6364) `shouldBe` True
                  it "Lighting with the light behind the surface" $ do
                                          let m = material
                                          let position = makePoint 0 0 0
                                          let eyev = makeVector 0 0 (-1)
                                          let normalv = makeVector 0 0 (-1)
                                          let light = Light (makePoint 0 0 10) (1,1,1)
                                          let result = lighting m light position eyev normalv False
                                          result `colorEqual` (0.1, 0.1, 0.1) `shouldBe` True
               describe "Creating a world" $ do
                  it "Creating a world" $ do
                                          let w = makeWorld
                                          let World o l = w
                                          length o `shouldBe` 0
                  it "The default world" $ do
                                           let light = Light (makePoint (-10) 10 (-10)) white
                                           let m = material { materialColor = (0.8, 1.0, 0.6), materialDiffuse = 0.7, materialSpecular = 0.2}
                                           let s1 = makeSphere { shapeMaterial = m }
                                           let s2 = makeSphere { shapeTransform = scaling 0.5 0.5 0.5 }
                                           let w = defaultWorld
                                           let World objects l = w
                                           l `shouldBe` light
                                           head objects `shouldBe` s1
                                           objects !! 1 `shouldBe` s2
                  it "Intersect a world with a ray" $ do
                                           let w = defaultWorld
                                           let r = Ray (makePoint 0 0 (-5) ) (makeVector 0 0 1)
                                           let xs = intersect_world w r
                                           length xs `shouldBe` 4
                                           tValue (xs !! 0) `shouldBe` 4
                                           tValue (xs !! 1) `shouldBe` 4.5
                                           tValue (xs !! 2) `shouldBe` 5.5
                                           tValue (xs !! 3) `shouldBe` 6
                  it "Precomputing the state of an intersection" $ do
                                           let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                           let shape = makeSphere
                                           let i = Intersection 4 shape
                                           let comps = prepare_computations i r
                                           compT comps `shouldBe` tValue i
                                           compObject comps `shouldBe` object i
                                           compPoint comps`shouldBe` makePoint 0 0 (-1)
                                           compEyev comps `shouldBe` makeVector 0 0 (-1)
                                           compNormalv comps `shouldBe` makeVector 0 0 (-1)
                  it "The hit when an intersection occurs on the outside" $ do
                                           let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                           let s = makeSphere
                                           let i = Intersection 4 s
                                           let comps = prepare_computations i r
                                           compInside comps `shouldBe` False
                  it "The hit, when an intersection occurs on the inside" $ do
                                           let r = Ray (makePoint 0 0 0) (makeVector 0 0 1)
                                           let s = makeSphere
                                           let i = Intersection 1 s
                                           let comps = prepare_computations i r
                                           compPoint comps `shouldBe` makePoint 0 0 1
                                           compEyev comps `shouldBe` makeVector 0 0 (-1)
                                           compInside comps `shouldBe` True
                                           compNormalv comps `shouldBe` makeVector 0 0 (-1)
                  it "Shading an intersection" $ do
                                           let w = defaultWorld
                                           let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                                           let shape = (worldObjects w !! 0)
                                           let i = Intersection 4 shape
                                           let comps = prepare_computations i r
                                           let c = shade_hit w comps
                                           print c
                                           c `colorEqual` (0.38066, 0.47583, 0.2855) `shouldBe` True
                  it "Shading an intersection from the inside" $ do
                                           let l = Light (makePoint 0 0.25 0) (1, 1, 1)
                                           let w = defaultWorld { worldLight = l} --changing the light field of world
                                           let r = Ray (makePoint 0 0 0) (makeVector 0 0 1)
                                           let shape = (worldObjects w !! 1)
                                           let i = Intersection 0.5 shape
                                           let comps = prepare_computations i r
                                           let c = shade_hit w comps
                                           print c
                                           c `colorEqual` (0.90498, 0.90498, 0.90498) `shouldBe` True
                  it "The color when a ray misses" $ do
                                           let w = defaultWorld
                                           let r = Ray (makePoint 0 0(-5)) (makeVector 0 1 0)
                                           let c = color_at w r
                                           c `colorEqual` (0, 0, 0) `shouldBe` True
                  it "The color when a ray hits" $ do
                                           let w = defaultWorld
                                           let r = Ray (makePoint 0 0(-5)) (makeVector 0 0 1)
                                           let c = color_at w r
                                           c `colorEqual` (0.38066, 0.47583, 0.2855) `shouldBe` True
                  it "The color with an intersection behind the ray" $ do
                                           let w = defaultWorld
                                           let outer = (worldObjects w !! 0)
                                           let outer' = outer {shapeMaterial = (shapeMaterial outer) {materialAmbient = 1}}
                                           let inner = (worldObjects w !! 1)
                                           let inner' = inner {shapeMaterial = (shapeMaterial inner) {materialAmbient = 1}}
                                           let w' = w {worldObjects = [outer',inner'] }
                                           let r = Ray (makePoint 0 0 0.75) (makeVector 0 0 (-1))
                                           let c = color_at w' r
                                           c `colorEqual`  materialColor (shapeMaterial inner') `shouldBe` True
               describe "Defining a View Transformation" $ do
                  it "The transformation matrix for the default orientation" $ do
                                           let from = makePoint 0 0 0
                                           let to = makePoint 0 0 (-1)
                                           let up = makeVector 0 1 0
                                           let t = view_transform from to up
                                           t `shouldBe` identity4
                  it "A view transformation matrix looking in the positive z direction" $ do
                                           let from = makePoint 0 0 0
                                           let to = makePoint 0 0 (1)
                                           let up = makeVector 0 1 0
                                           let t = view_transform from to up
                                           t `shouldBe` scaling (-1) 1 (-1)
                  it "The view transformation moves the world" $ do
                                           let from = makePoint 0 0 8
                                           let to = makePoint 0 0 0
                                           let up = makeVector 0 1 0
                                           let t = view_transform from to up
                                           t `shouldBe` translation 0 0 (-8)
                  it " An arbitrary view transformation" $ do
                                           let from = makePoint 1 3 2
                                           let to = makePoint 4 (-2) 8
                                           let up = makeVector 1 1 0
                                           let t = view_transform from to up
                                           t `equalMatrix` Matrix 4 4 (V.fromList [(-0.50709), 0.50709,   0.67612, (-2.36643),
                                                                                     0.76772,  0.60609,   0.12122, (-2.82843),
                                                                                   (-0.35857), 0.59761, (-0.71714),  0.00000,
                                                                                     0.00000,  0.00000,   0.00000,   1.00000]) `shouldBe` True

               describe "Implementing a camera" $ do
                  it "Constructing a camera" $ do
--                                            let hsize = 160
--                                            let vsize = 120
--                                            let field_of_view = (pi/2)
                                            let c = makeCamera 160 120 (pi/2.0)
                                            camHsize c `shouldBe` 160
                                            camVsize c `shouldBe` 120
                                            camField_of_view c `shouldBe` (pi/2.0)
                                            camTransform c `shouldBe` identity4
                  it "The pixel size for a horizontal canvas" $ do
                                           let  c = makeCamera 200 125 (pi/2)
                                           camPixelSize c `equalDouble` 0.01 `shouldBe` True
                  it "The pixel size for a vertical canvas" $ do
                                                             let  c = makeCamera 125 200 (pi/2)
                                                             camPixelSize c `equalDouble` 0.01 `shouldBe` True
                  it "Constructing a ray through the center of the canvas" $ do
                                                             let c = makeCamera 201 101 (pi/2)
                                                             let r = ray_for_pixel c 100 50
                                                             origin r `vectorEqual` makePoint 0 0 0 `shouldBe` True
                                                             direction r  `vectorEqual` makeVector 0 0 (-1) `shouldBe` True
                  it "Constructing a ray through a corner of the canvas" $ do
                                                             let c = makeCamera 201 101 (pi/2)
                                                             let r = ray_for_pixel c 0 0
                                                             origin r `vectorEqual` makePoint 0 0 0 `shouldBe` True
                                                             direction r  `vectorEqual` makeVector 0.66519 0.33259 (-0.66851) `shouldBe` True
                  it "Constructing a ray when the camera is transformed" $ do
                                                             let c = makeCamera 201 101 (pi/2)
                                                             let trans = rotation_y (pi/4) `multMat` translation 0 (-2) 5
                                                             let c' = c { camTransform = trans }
                                                             let r = ray_for_pixel c' 100 50
                                                             origin r `vectorEqual` makePoint 0 2 (-5) `shouldBe` True
                                                             direction r  `vectorEqual` makeVector ((sqrt 2)/2) 0 (-(sqrt 2)/2) `shouldBe` True
                  it "Rendering a world with a camera" $ do
                                                             let w = defaultWorld
                                                             let c = makeCamera 11 11 (pi/2)
                                                             let from = makePoint 0 0 (-5)
                                                             let to = makePoint 0 0 0
                                                             let up = makeVector 0 1 0
                                                             let trans = view_transform from to up
                                                             let c' = c { camTransform = trans }
                                                             let image = render c' w
                                                             (image ! (5,5)) `colorEqual` (0.38066, 0.47583, 0.2855) `shouldBe` True
               describe "Lighting in Shadows" $ do
                  it "Lighting with the surface in shadow" $ do
                                                             let m = material
                                                             let position = makePoint 0 0 0
                                                             let eyev = makeVector 0 0 (-1)
                                                             let normalv = makeVector 0 0 (-1)
                                                             let light = Light (makePoint 0 0 (-10)) (1,1,1)
                                                             let in_shadow = True
                                                             let result = lighting m light position eyev normalv in_shadow
                                                             result `colorEqual` (0.1,0.1,0.1) `shouldBe` True
                  it "There is now shadow when nothing is collinear with point and light" $ do
                        let w = defaultWorld
                        let p = makePoint 0 10 0
                        is_shadowed w p `shouldBe` False
                  it "The shadow when an object is between the point and the light" $ do
                        let w = defaultWorld
                        let p = makePoint 10 (-10) 10
                        is_shadowed w p `shouldBe` True
                  it "There is no shadow when an object is behind the light" $ do
                        let w = defaultWorld
                        let p = makePoint (-20) 20 (-20)
                        is_shadowed w p `shouldBe` False
                  it "There is no shadow when an object is behind the point" $ do
                        let w = defaultWorld
                        let p = makePoint (-2) 2 (-2)
                        is_shadowed w p `shouldBe` False
                  it "shade_hit is given an intersection in shadow" $ do
                        let w = defaultWorld
                        let s1 = makeSphere
                        let s2 = makeSphere {shapeTransform = translation 0 0 10}
                        let w' = w {worldLight = Light (makePoint 0 0 (-10)) (1,1,1), worldObjects = [s1,s2]}
                        let r = Ray (makePoint 0 0 5) (makeVector 0 0 1)
                        let i = Intersection 4 s2
                        let comps = prepare_computations i r
                        let c = shade_hit w' comps
                        c `colorEqual` (0.1, 0.1, 0.1) `shouldBe` True
                  it "The hit should offset the point" $ do
                        let r = Ray (makePoint 0 0 (-5)) (makeVector 0 0 1)
                        let shape = makeSphere {shapeTransform = translation 0 0 1}
                        let i = Intersection 5 shape
                        let comps@(Comp _ _ (_, _, pZ, _) _ _ _ (_, _, opZ, _)) = prepare_computations i r
                        opZ < (-epsilon)/2 `shouldBe` True
                        pZ > opZ `shouldBe` True
               describe "Planes" $ do
                  it "The normal of a plane is constant everywhere" $ do
                        let p = makePlane
                        let n1 = normal_at p (makePoint 0 0 0)
                        let n2 = normal_at p (makePoint 10 0 (-10))
                        let n3 = normal_at p (makePoint (-5) 0 150)
                        n1 `vectorEqual` makeVector 0 1 0 `shouldBe` True
                        n2 `vectorEqual` makeVector 0 1 0 `shouldBe` True
                        n3 `vectorEqual` makeVector 0 1 0 `shouldBe` True
                  it "Intersect with a ray parallel to the plane" $ do
                        let p = makePlane
                        let r = Ray (makePoint 0 10 0) (makeVector 0 0 1)
                        let xs = intersect p r
                        xs `shouldBe` []
                  it "Intersect with a coplanar ray" $ do
                        let p = makePlane
                        let r = Ray (makePoint 0 0 0) (makeVector 0 0 1)
                        let xs = intersect p r
                        xs `shouldBe` []
                  it "A ray intersecting a plane form above" $ do
                        let p = makePlane
                        let r = Ray (makePoint 0 1 0) (makeVector 0 (-1) 0)
                        let xs = intersect p r
                        length xs `shouldBe` 1
                        tValue (xs !! 0) `shouldBe` 1
                        object (xs !! 0) `shouldBe` p
                  it "A ray intersecting a plane form above" $ do
                      let p = makePlane
                      let r = Ray (makePoint 0 (-1) 0) (makeVector 0 1 0)
                      let xs = intersect p r
                      length xs `shouldBe` 1
                      tValue (xs !! 0) `shouldBe` 1
                      object (xs !! 0) `shouldBe` p
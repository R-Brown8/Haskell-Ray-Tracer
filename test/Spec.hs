import Test.Hspec
import Lib
import Data.Array.Unboxed

main :: IO ()
main = hspec $ do
               describe "Tuples" $ do
                  it "testing hspec" $ do
                     True `shouldBe` True
                  it "A tuple with w=1.0 is a point" $ do
                    let a = makePoint 4.3 (-4.2) 3.1
                    let (x,y,z,w) = a
                    x `shouldBe` 4.3
                    y `shouldBe` (-4.2)
                    z `shouldBe` 3.1
                    w `shouldBe` 1.0
                  it "A tuple with w=0 is a vector" $ do
                   let a = makeVector 4.3 (-4.2) 3.1
                   let (x,y,z,w) = a
                   x `shouldBe` 4.3
                   y `shouldBe` (-4.2)
                   z `shouldBe` 3.1
                   w `shouldBe` 0
                  it "Adding two tuples" $ do
                    let a1 = makePoint 3 (-2) 5
                    let a2 = makeVector (-2) 3 1
                    a1 `addT` a2 `shouldBe` (makePoint 1 1 6)
                  it "Subtracting two points" $ do
                   let p1 = makePoint 3 2 1
                   let p2 = makePoint 5 6 7
                   p1 `subP` p2 `shouldBe` (makeVector (-2) (-4) (-6))
                  it "Subtracting a vector from a point" $ do
                                     let p1 = makePoint 3 2 1
                                     let v1 = makeVector 5 6 7
                                     p1 `subP` v1 `shouldBe` (makePoint (-2) (-4) (-6))
                  it "Subtracting two vectors" $ do
                                     let v1 = makeVector 3 2 1
                                     let v2 = makeVector 5 6 7
                                     v1 `subP` v2 `shouldBe` (makeVector (-2) (-4) (-6))
                  it "Negating a tuple" $ do
                                     let a = makeVector 1 (-2) 3
                                     let b = negateV a
                                     b `shouldBe` (makeVector (-1) 2 (-3))
-- Scalar Multiplication and Division
                  it "Multiplying a tuple by a scalar" $ do
                                    let a = (1, (-2), 3, (-4))
                                    let b = a `multVector` 3.5
                                    b `shouldBe` ( 3.5, (-7), 10.5,(-14))
                  it "Multiplying a tuple by a fraction" $ do
                                    let a =  (1, (-2), 3, (-4))
                                    let b = a `multVector` 0.5
                                    b ` shouldBe`(0.5, (-1), 1.5,(-2))
--Magnitude
                  it "Computing the Magnitude of a Vector(1,0,0)" $ do
                                    let a = makeVector 1 0 0
                                    magnitude a `shouldBe` 1

                  it "Computing the Magnitude of a Vector(0,1,0)" $ do
                                                      let a = makeVector 0 1 0
                                                      magnitude a `shouldBe` 1
                  it "Computing the Magnitude of a Vector(0,0,1)" $ do
                                                      let a = makeVector 0 0 1
                                                      magnitude a `shouldBe` 1
                  it "Computing the Magnitude of a Vector(1,2,3)" $ do
                                                      let a = makeVector 1 2 3
                                                      magnitude a `shouldBe` sqrt(14)
                  it "Computing the Magnitude of a Vector(-1,-2,-3)" $ do
                                                      let a = makeVector (-1) (-2) (-3)
                                                      magnitude a `shouldBe` sqrt(14)
--Normalizing
                  it "Normalizing a vector(4,0,0) gives (1,0,0)" $ do
                                                      let a = makeVector 4 0 0
                                                      normalize a `shouldBe` makeVector 1 0 0
--Dot product
                  it "The dot product of two tuples" $ do
                                                     let a = makeVector 1 2 3
                                                     let b = makeVector 2 3 4
                                                     dot a b `shouldBe` 20
--cross product
                  it "The cross product of two vectors" $ do
                                                      let a = makeVector 1 2 3
                                                      let b = makeVector 2 3 4
                                                      cross a b `shouldBe` makeVector (-1) 2 (-1)
                                                      cross b a `shouldBe` makeVector 1 (-2) 1
--Representing Colors
               describe "Colors" $ do
                  it "Colors are (red, green, blue) tuples" $ do
                      let c = ((-0.5), 0.4, 1.7) :: Color --c is tuple of type color
                      let (red,green,blue) = c
                      red `shouldBe` (-0.5)
                      green `shouldBe` 0.4
                      blue `shouldBe` 1.7

--Implementing Color Operations
                  it "Adding Colors" $ do
                    let c1 = (0.9, 0.6, 0.75) :: Color
                    let c2 = (0.7, 0.1, 0.25) :: Color
                    c1 `addColor` c2 `shouldBe` (1.6, 0.7, 1.0)
--                  it "Subtracting Colors" $ do
--                    let c1 = (0.9, 0.6, 0.75) :: Color
--                    let c2 = (0.7, 0.1, 0.25) :: Color
--                    c1 `subColor` c2 `shouldBe` (0.2,0.5,0.5)
                  it "Multiplying a color by a scalar" $ do
                     let c = (0.2, 0.3, 0.4) :: Color
                     c `multColorScalar` 2 `shouldBe` (0.4,0.6, 0.8)
--                  it "Multiplying colors" $ do
--                     let c1 = (1, 0.2, 0.4) :: Color
--                     let c2 = (0.9,1, 0.1) :: Color
--                     c1 `multColor` c2 `shouldBe` (0.9, 0.2, 0.04)

--Creating a Canvas
               describe "Canvas" $ do
                  it "Creating a canvas" $ do
                     let c = makeCanvas 10 20
                     let (_, (y,x)) = bounds c
                     x+1 `shouldBe` 10
                     y+1 `shouldBe` 20
--                     let elems = elems c
--                     print elems
                  it "writing pixels to a canvas" $ do
                      let c = makeCanvas 10 20
                      let red = (1, 0, 0)
                      let c' = updateCanvas c [((2,3),red)]
                      let p = c' ! (2,3)
                      p `shouldBe` red


















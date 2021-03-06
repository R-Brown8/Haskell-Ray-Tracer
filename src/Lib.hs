{-# LANGUAGE OverloadedStrings #-}
-- stack ghci --ghci-options -isrc --ghci-options -itest ray-tracer:ray-tracer-test
module Lib
       where
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.STRef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word8
import qualified Data.List as L
import System.Random
import Data.Maybe
import Data.Either
import Data.Sort
import Data.Ord
import Math.Extras.Double
import Data.Fixed
import qualified Numeric.Limits as L
import Data.Function.Memoize
import qualified Data.Vector as V
import Lens.Micro.Platform
import qualified Data.Matrix as M


someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Color = (Double, Double, Double)
type Canvas = Array (Int, Int) (Color)
type Point = (Double, Double, Double, Double)
type Vector = (Double, Double, Double, Double)
type Origin = Point
type Direction = Vector


data Projectile = Projectile Point Vector deriving (Show)

data Environment = Environment Vector Vector deriving (Show)

data PointLight = Light {lightPoint :: Point, lightIntensity :: Color} deriving (Eq, Show)

data Material = Material {
  materialPattern :: Pattern,
  materialColor :: Color,
  materialAmbient :: Double,
  materialDiffuse :: Double,
  materialSpecular :: Double,
  materialShininess :: Double,
  materialReflective :: Double,
  materialTransparent :: Double,
  materialRefractive :: Double } deriving (Eq, Show)

data Matrix = Matrix Int Int (V.Vector Double) deriving (Eq, Show)

data Intersection = Intersection {
    tValue :: Double,
    object :: Shape
  } deriving (Eq, Show)

data Ray = Ray {
    origin :: Point,
    direction :: Vector
    } deriving (Eq, Show)

data Shape = Sphere {
    shapeID :: Int,
    shapeTransform :: Matrix,
    shapeMaterial :: Material
    }
    | Plane {
    shapeId :: Int,
    shapeTransform :: Matrix,
    shapeMaterial :: Material
    }
    | Cube {
    shapeId :: Int,
    shapeTransform :: Matrix,
    shapeMaterial :: Material
    }
    | Cylinder {
    shapeId :: Int,
    shapeTransform :: Matrix,
    shapeMaterial :: Material,
    shapeMinimum :: Double,
    shapeMaximum :: Double,
    shapeClosed :: Bool
    }
    | Cone {
    shapeId :: Int,
    shapeTransform :: Matrix,
    shapeMaterial :: Material,
    shapeMinimum :: Double,
    shapeMaximum :: Double,
    shapeClosed :: Bool
    }
    | TestShape {
    shapeId :: Int,
    shapeTransform :: Matrix,
    shapeMaterial :: Material
    } deriving (Eq, Show)

data World = World {
     worldObjects :: [Shape],
     worldLight :: PointLight
      }

data Comps = Comp {
     compT :: Double,
     compObject :: Shape,
     compPoint :: Point,
     compEyev :: Vector,
     compNormalv :: Vector,
     compInside :: Bool,
     compOverPoint :: Point,
     compUnderPoint :: Point,
     compReflect :: Vector,
     compsN1 :: Double,
     compsN2 :: Double
}

data Camera = Camera {
     camHsize :: Int,
     camVsize :: Int,
     camField_of_view :: Double,
     camTransform :: Matrix,
     camHalfWidth :: Double,
     camHalfHeight :: Double,
     camPixelSize :: Double
} deriving (Eq,Show)

data Pattern = NoPattern {
     color1 :: Color,
     color2 :: Color,
     patternTransform :: Matrix
} | StripePattern {
     color1 :: Color,
     color2 :: Color,
     patternTransform :: Matrix
} | GradientPattern {
     color1 :: Color,
     color2 :: Color,
     patternTransform :: Matrix
} | RingPattern {
     color1 :: Color,
     color2 :: Color,
     patternTransform :: Matrix
} | CheckersPattern {
     color1 :: Color,
     color2 :: Color,
     patternTransform :: Matrix
} | TestPattern {
    color1 :: Color,
    color2 :: Color,
    patternTransform :: Matrix
} deriving (Eq, Show)

makePoint :: Double -> Double -> Double -> Point
makePoint x y z = (x,y,z,1.0)

makeVector :: Double -> Double -> Double -> Vector
makeVector x y z = (x,y,z,0)

equalDouble :: Double -> Double -> Bool
equalDouble x y = abs(x-y) < epsilon


epsilon = 0.0001
recursiveDepth = 5
infinity = read "Infinity"

defaultWorld = let light = Light (makePoint (-10) 10 (-10)) white
                   m = material { materialColor = (0.8, 1.0, 0.6), materialDiffuse = 0.7, materialSpecular = 0.2}
                   s1 = makeSphere { shapeMaterial = m }
                   s2 = makeSphere { shapeTransform = scaling 0.5 0.5 0.5 } in
               World [s1,s2] light

material :: Material
material = Material { materialPattern = NoPattern white black identity4,
                      materialColor = white, materialAmbient = 0.1,
                      materialDiffuse = 0.9, materialSpecular = 0.9,
                      materialShininess = 200.0,
                      materialReflective = 0.0,
                      materialTransparent = 0.0,
                      materialRefractive = 1.0}


multVector (x1,y1,z1,w1) x = (x1*x,y1*x,z1*x,w1*x)

addT (x1,y1,z1,w1) (x2,y2,z2,w2) = (x1+x2, y1+y2, z1+z2, w1+w2)
subP (x1,y1,z1,w1) (x2,y2,z2,w2) = (x1-x2, y1-y2, z1-z2, w1-w2)

negateV (x1,y1,z1,_) = ((-x1),(-y1),(-z1),0)

magnitude (x1, y1, z1, w1) = sqrt(x1^2 + y1^2 + z1^2 + w1^2)

normalize v@(x1,y1,z1,w1) = let m = magnitude v in
                            (x1/m, y1/m, z1/m, w1/m)

dot (x1,y1,z1,w1) (x2,y2,z2,w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2

cross (x1,y1,z1,w1) (x2,y2,z2,w2) = makeVector (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)



p = Projectile (makePoint 0 1 0) (normalize(makeVector 1 1 0))

e = Environment (makeVector 0 (-0.1) 0) (makeVector (-0.01) 0 0)

tick :: Environment -> Projectile -> Projectile
tick e p = let Projectile pos vel = p
               Environment grav wind = e
               newPos = pos `addT` vel
               newVel = vel `addT` grav `addT` wind in
           Projectile newPos newVel

runTick :: Environment -> Projectile  -> [(Double,Double)] -> [(Double,Double)]
runTick environment projectile array = do
          let p1 = tick environment projectile
          let Projectile (x1, y1, z1, w1)  _ = p1
          if y1 > 0
          then runTick environment p1 ((x1,y1):array)
          else array


addColor (r1,g1,b1) (r2,g2,b2) = (r1+r2, g1+g2, b1+b2)

subColor (r1,g1,b1) (r2,g2,b2) = (r1-r2, g1-g2, b1-b2)

multColorScalar (r,g,b) x = (r*x,g*x,b*x)

multColor (r1,g1,b1) (r2,g2,b2) = (r1*r2, g1*g2, b1*b2)

black = (0,0,0) :: Color
red = (1,0,0) :: Color
white = (1,1,1) :: Color
blue = (0,0,1) :: Color

makeCanvas :: Int -> Int -> Canvas
makeCanvas width height = array ((0,0), (height-1,width-1))
                                [((y, x), black) | x <- [0..(width-1)],
                                y <- [0 .. (height-1)]]

updateCanvas :: Canvas -> [((Int, Int), Color)] -> Canvas
updateCanvas canvas ptCls =
                                let (w,h) = (snd . bounds) canvas in
                                runSTArray $ do
                                               stArray <- thaw canvas
                                               forM_ ptCls (\(coord, color) -> do
                                                                                let (y,x) = coord
                                                                                writeArray stArray (x,y) color
                                                                                )
                                               return stArray

parabProjectile = let start = makePoint 0 1 0
                      vel = normalize (makeVector 1 1.8 0) `multVector` 11.25
                      p = Projectile start vel
                      gravity = makeVector 0 (-0.1) 0
                      wind = makeVector (-0.01) 0 0
                      e = Environment gravity wind
                      c = makeCanvas 900 550
                      xs = runTick e p []
                      pointColors = map (\(x, y) -> (((550- round y), round x), red)) xs
                      updatedCanvas = updateCanvas c pointColors in
                  updatedCanvas

saveProjectile = saveCanvasToFile "projectile.ppm" parabProjectile

groupColors :: Int -> [T.Text] -> T.Text
groupColors len colors = case colors of
                          [] -> ""
                          _ -> mconcat $ [mconcat ((take len colors) ++ ["\n"])] ++ [(groupColors len (drop len colors))]

clamp255 ::Double -> Int
clamp255 n = let n' = toRational n in
             if n' > 1
             then 255
             else if n' <= 0
                  then 0
                  else floor (n' * 255)


canvasToPPM :: Canvas -> T.Text
canvasToPPM colorArray = ppmText
     where (y,x) = (snd . bounds) colorArray
           -- colors = map ((\c -> (channelRed c, channelGreen c, channelBlue c)) . toSRGB24) $ elems colorArray
           colors = elems colorArray
           colorText = map (\(r, g, b) -> mconcat [(T.pack . show . clamp255) r, " ", (T.pack . show . clamp255) g, " ", (T.pack . show . clamp255) b, " "]) colors
           colorTextLines = groupColors (x+1) colorText
           ppmText = mconcat ["P3\n"
                              ,(T.pack . show) $ x + 1
                              ," "
                              ,(T.pack . show) $ y + 1
                              ,"\n"
                              ,(T.pack . show) 255
                              ,"\n"
                              ,colorTextLines]

saveCanvas colorArray = TIO.writeFile "raytrace.ppm" (canvasToPPM colorArray)

saveCanvasToFile fileName colorArray = TIO.writeFile fileName (canvasToPPM colorArray)


--Matrix work

fromMatrix :: Matrix -> M.Matrix Double
fromMatrix (Matrix w h v) = M.fromList w h (V.toList v)

toMatrix :: M.Matrix Double -> Matrix
toMatrix m =  Matrix (M.nrows m) (M.ncols m) (V.fromList $ M.toList m)

identity4 = Matrix 4 4 (V.fromList [1, 0, 0, 0,
                                    0, 1, 0, 0,
                                    0, 0, 1, 0,
                                    0, 0, 0, 1])

translation :: Double -> Double -> Double -> Matrix
translation x y z = Matrix 4 4 (V.fromList [1, 0, 0, x,
                                            0, 1, 0, y,
                                            0, 0, 1, z,
                                            0, 0, 0, 1])

scaling :: Double -> Double -> Double -> Matrix
scaling x y z = Matrix 4 4 (V.fromList [x, 0, 0, 0,
                                        0, y, 0, 0,
                                        0, 0, z, 0,
                                        0, 0, 0, 1])
rotation_x :: Double -> Matrix
rotation_x r = Matrix 4 4 (V.fromList [ 1, 0 ,0, 0,
                                        0, cos r, (-sin r), 0,
                                        0, sin r,   cos r,  0,
                                        0, 0,       0,      1])
rotation_y :: Double -> Matrix
rotation_y r = Matrix 4 4 (V.fromList [ cos r,    0, sin r, 0,
                                        0,        1, 0,     0,
                                        (-sin r), 0, cos r, 0,
                                        0,        0, 0,     1])

rotation_z :: Double -> Matrix
rotation_z r = Matrix 4 4 (V.fromList [ cos r,    (-sin r), 0, 0,
                                        sin r,    cos r,    0, 0,
                                        0,        0,        1, 0,
                                        0,        0,        0, 1])
shearing :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
shearing x1 x2 y1 y2 z1 z2 = Matrix 4 4 (V.fromList [ 1, x1, x2, 0,
                                                      y1, 1, y2, 0,
                                                      z1, z2, 1, 0,
                                                      0,  0,  0, 1])


search :: Matrix -> Int -> Int -> Double
search m row column = let m' = fromMatrix m in
                    m' M.! (row,column)


equalMatrix :: Matrix -> Matrix -> Bool
equalMatrix (Matrix r c m) (Matrix r1 c1 m1) =  if (r /= r1 || c /= c1)
                                                then False
                                                else let zs = V.zip m m1 in
                                                      foldl (\eq (v1, v2) -> if eq
                                                                         then equalN v1 v2
                                                                         else False) True zs

transpose :: Matrix -> Matrix
transpose (Matrix r c m) = Matrix c r $ V.fromList $ concat $ map (\row -> map (\col -> m V.! ((col*(r)) + row)) [0 .. c-1]) [0 .. r-1]

transform :: Matrix -> Ray -> Ray --multiply matrix by array and return vector
transform m1 (Ray pt vr) = let pt' = m1 `multMatVector` pt
                               vr' = m1 `multMatVector` vr in
                               Ray pt' vr'


multMat :: Matrix -> Matrix -> Matrix
multMat m1 m2 = let m1' = fromMatrix m1
                    m2' = fromMatrix m2
                    mx = m1' `M.multStrassen` m2' in
                toMatrix mx

multMatVector :: Matrix -> Vector -> Vector
multMatVector m1 (a, b, c, w) = let (Matrix _ _ mv) = m1 `multMat` (Matrix 4 1 (V.fromList [a, b, c, w])) in
                                        ( (mv V.! 0), (mv V.! 1), (mv V.! 2), (mv V.! 3) )

submatrix :: Matrix -> Int -> Int -> Matrix
submatrix mat row col = toMatrix $ M.minorMatrix row col (fromMatrix mat)

determinant :: Matrix -> Double
determinant mat = M.detLaplace $ fromMatrix mat


minor :: Matrix -> Int -> Int -> Double
minor m row col = determinant $ submatrix m row col

cofactor m row col = let mn = minor m row col in
                     if (((row + col) `mod` 2) == 0)
                     then mn
                     else -mn

inverse :: Matrix -> Matrix
inverse mat = let me = M.inverse $ fromMatrix mat in
              case me of
                Left msg -> error msg
                Right m' -> toMatrix m'

equalN :: Double -> Double -> Bool
equalN x y = abs (x - y) <= epsilon

equalV :: [Double] -> [Double] -> Bool
equalV v1 v2 = let l1 = length v1
                   l2 = length v2 in
                   if (l1 /= l2)
                   then False
                   else let ds = zip v1 v2 in
                        foldl (\eq (d1, d2) -> if eq
                                               then equalN d1 d2
                                               else False) True ds

vectorEqual :: Vector -> Vector -> Bool
vectorEqual (x, y, z, w) (x2, y2, z2, w2) = x `equalN` x2 && y `equalN` y2 && z `equalN` z2 && w `equalN` w2

colorEqual :: Color -> Color -> Bool
colorEqual (r, g, b) (r2, g2, b2) = r `equalN` r2 && g `equalN` g2 && b `equalN` b2

combTrans :: [Matrix] -> Matrix
combTrans xs = foldl (\i m -> i `multMat` m) identity4 xs

rayPosition :: Ray -> Double -> Point
rayPosition (Ray orig dir) t = orig `addT` (dir `multVector` t)

makeSphere :: Shape
makeSphere = Sphere 5 identity4 material

glassSphere :: Shape
glassSphere = Sphere 5 identity4 (material { materialTransparent = 1.0, materialRefractive = 1.5})

makePlane :: Shape
makePlane = Plane 5 identity4 material

makeCube :: Shape
makeCube = Cube 5 identity4 material

makeCylinder :: Shape
makeCylinder = Cylinder 5 identity4 material (-infinity) infinity False

makeCone :: Shape
makeCone = Cone 5 identity4 material (-infinity) infinity False
makeWorld :: World
makeWorld = World [] (Light (makePoint 0 0 0) white)

makeCamera :: Int -> Int -> Double -> Camera
makeCamera hsize vsize field_of_view = let half_view = tan (field_of_view/2)
                                           aspect = ( (fromIntegral hsize)/ (fromIntegral vsize) )
                                           (halfWidth, halfHeight) = if aspect >= 1 then (half_view, half_view/aspect) else (half_view*aspect, half_view)
                                           pixelSize = (halfWidth*2)/ (fromIntegral hsize) in
                                       Camera hsize vsize field_of_view identity4 halfWidth halfHeight pixelSize


intersect_world :: World -> Ray -> [Intersection]
intersect_world world ray = let shapes = worldObjects world in
                                intersections $ foldl (\acc s -> (intersect s ray) ++ acc)  [] shapes -- returning sorted foldl

prepare_computations :: Intersection -> Ray -> [Intersection] -> Comps
prepare_computations inters ray@(Ray orig dir) ixs = let t = tValue inters
                                                         s = object inters
                                                         p = rayPosition ray t
                                                         eyev = negateV dir
                                                         dotNE = dot normalv eyev
                                                         normalv = normal_at s p
                                                         normalv' = if dotNE < 0
                                                                    then negateV normalv
                                                                    else normalv
                                                         reflectv = reflect dir normalv'
                                                         compsOP = p `addT` (normalv' `multVector` epsilon)
                                                         compsUP = p `subP` (normalv' `multVector` epsilon)
                                                         inside = dotNE < 0
                                                         (n1, n2, _, _ ) = foldl (\acc@(n1, n2, xs, stop) ix -> if stop
                                                                                                            then (n1, n2, xs, stop)
                                                                                                            else let stop' = ix == inters
                                                                                                                     n1' = if ix == inters && length xs == 0
                                                                                                                           then 1.0
                                                                                                                           else materialRefractive (shapeMaterial ((head . reverse) xs) )
                                                                                                                     xs' = if object ix `elem` xs
                                                                                                                           then filter (\shape -> (object ix) /= shape ) xs
                                                                                                                           else xs ++ [object ix]
                                                                                                                     n2' = if ix == inters && length xs' == 0
                                                                                                                           then 1.0
                                                                                                                           else materialRefractive (shapeMaterial ((head . reverse) xs') ) in
                                                                                                                     (n1', n2', xs', stop')
                                                                                                      )  (1, 1, [], False) ixs

                                                         in
                                                     Comp t s p eyev normalv' inside compsOP compsUP reflectv n1 n2

intersect :: Shape -> Ray -> [Intersection]
intersect s@(Sphere _ _ _) r =  let trx = shapeTransform s
                                    Ray orig dir = transform (inverse trx) r
                                    sphere_to_ray = orig `subP` makePoint 0 0 0
                                    a = dot dir dir
                                    b = 2 * (dot dir sphere_to_ray)
                                    c = (dot sphere_to_ray sphere_to_ray) - 1
                                    discriminant = b^2 - (4 * a * c) in
                                if discriminant < 0
                                then []
                                else let t1 = (((-b) - sqrt discriminant) / (2*a) )
                                         t2 = (((-b) + sqrt discriminant) / (2*a) ) in
                                         [Intersection t1 s , Intersection t2 s]
intersect p@(Plane _ _ _) r = let trx = shapeTransform p
                                  Ray (origX, origY, origZ, origW) (dirX,dirY,dirZ,dirw) = transform (inverse trx) r in
                              if abs dirY < epsilon
                              then []
                              else let t = (-origY)/dirY in
                                   [Intersection t p]
intersect c@(Cube _ _ _) r = let trx = shapeTransform c
                                 Ray (xP,yP,zP,wP) (xV,yV,zV,wV) = transform (inverse trx) r
                                 (xTmin, xTmax) = check_axis xP xV
                                 (yTmin, yTmax) = check_axis yP yV
                                 (zTmin, zTmax) = check_axis zP zV
                                 tMin = maximum [xTmin, yTmin, zTmin]
                                 tMax = minimum [xTmax, yTmax, zTmax] in
                             if tMin > tMax
                             then []
                             else [Intersection tMin c, Intersection tMax c]
intersect cyl@(Cylinder _ _ _ _ _ _) r = let trx = shapeTransform cyl
                                             Ray (xP,yP,zP,wP) (xV,yV,zV,wV) = transform (inverse trx) r
                                             a = xV^2 + zV^2
                                             b = (2 * xP * xV) + (2 * zP * zV)
                                             c = xP^2 + zP^2 - 1
                                             disc = b^2 - 4 * a * c in
                                   if (abs a) < epsilon || disc < 0
                                   then intersect_caps cyl r
                                   else let t0 = (-b - (sqrt disc)) / (2*a)
                                            t1 = (-b + (sqrt disc))/ (2*a)
                                            (t0',t1') = if t0 > t1
                                                        then (t1,t0)
                                                        else (t0,t1)
                                            y0 = yP + t0' * yV
                                            xs = if shapeMinimum cyl < y0 && y0 < shapeMaximum cyl
                                                 then [Intersection t0' cyl]
                                                 else []
                                            y1 = yP + t1' * yV
                                            xs' = if shapeMinimum cyl < y1 && y1 < shapeMaximum cyl
                                                  then [Intersection t1' cyl]
                                                  else [] in
                                       xs ++ xs' ++ (intersect_caps cyl r)
intersect cone@(Cone  _ _ _ _ _ _) r = let trx = shapeTransform cone
                                           Ray (xP,yP,zP,wP) (xV,yV,zV,wV) = transform (inverse trx) r
                                           a = xV^2 - yV^2 + xV^2
                                           b = (2 * xP * xV) - (2 * yP * yV) + (2 * zP * zV)
                                           c = xP^2 - yP^2 + zP^2
                                           disc = b^2 - 4 * a * c in
                                 if (abs a) < epsilon || disc < 0
                                 then []
                                 else let t0 = (-b - (sqrt disc)) / (2*a)
                                          t1 = (-b + (sqrt disc))/ (2*a)
                                          (t0',t1') = if t0 > t1
                                                      then (t1,t0)
                                                      else (t0,t1)
                                          y0 = yP + t0' * yV
                                          xs = if shapeMinimum cone < y0 && y0 < shapeMaximum cone
                                               then [Intersection t0' cone]
                                               else []
                                          y1 = yP + t1' * yV
                                          xs' = if shapeMinimum cone < y1 && y1 < shapeMaximum cone
                                                then [Intersection t1' cone]
                                                else [] in
                                     xs ++ xs'

intersect_caps :: Shape -> Ray -> [Intersection]
intersect_caps cyl ray@(Ray (xO, yO, zO, _) (xD, yD, zD, _)) = if shapeClosed cyl == False || (abs yD) < epsilon
                                                               then []
                                                               else let t = ((shapeMinimum cyl) - yO)  / yD
                                                                        xs = if check_cap ray t
                                                                             then [Intersection t cyl]
                                                                             else []
                                                                        t' = ((shapeMaximum cyl) - yO)  / yD
                                                                        xs' = if check_cap ray t'
                                                                              then [Intersection t' cyl]
                                                                              else [] in
                                                               xs ++ xs'
check_cap :: Ray -> Double -> Bool
check_cap ray@ (Ray (xO, yO, zO, _)(xD, yD, zD, _)) t = let x = xO + t * xD
                                                            z = zO + t * zD in
                                                            (x^2 + z^2) <= 1

check_axis :: Double -> Double ->(Double, Double)
check_axis origin direction = let tmin_numerator = ((-1) - origin)
                                  tmax_numerator = (1 - origin)
                                  (min, max) = if abs(direction) >= epsilon
                                               then  (tmin_numerator/direction, tmax_numerator/direction)
                                               else (tmin_numerator * infinity, tmax_numerator * infinity) in
                              if min > max
                              then (max, min)
                              else (min, max)




intersections :: [Intersection] -> [Intersection]
intersections i1 = sortBy (\(Intersection t1 _) (Intersection t2 _)  -> if t1 > t2
                                                                         then GT
                                                                         else if t1 < t2
                                                                              then LT
                                                                              else EQ) i1
shade_hit :: World -> Comps -> Int -> Color
shade_hit world comps remaining = let mat = shapeMaterial (compObject comps) --accesor for material of object
                                      l = worldLight world
                                      shadowed = is_shadowed world (compOverPoint comps)
                                      surface =  lighting mat (compObject comps) l (compOverPoint comps) (compEyev comps) (compNormalv comps) shadowed
                                      reflected = reflected_color world comps remaining --returns Color
                                      refracted = refracted_color world comps remaining --returns Color
                                      material = shapeMaterial (compObject comps)
                                      reflectance = schlick comps in --shlick returns Double
                                      if (materialReflective material) > 0 && (materialTransparent material) > 0
                                      then (surface `addColor` (reflected  `multColorScalar` reflectance)) `addColor` ( refracted `multColorScalar` (1 - reflectance) )
                                      else surface `addColor` reflected `addColor` refracted


--                                      surface `addColor` reflected `addColor` refracted


                                      --l = worldLight world
                                      --                                      shadowed = is_shadowed world (compOverPoint comps)
                                      --                                      reflected = reflected_color world comps remaining
                                      --                                      refracted = refracted_color world comps remaining
                                      --                                      surface =  lighting mat (compObject comps) l (compOverPoint comps) (compEyev comps) (compNormalv comps) shadowed in
                                      --                                      surface `addColor` reflected `addColor` refracted

schlick :: Comps -> Double
schlick comps = let (Comp _ compOb _ compEyev compNormalv _ compOP compUP compReflect compN1 compN2) = comps
                    cos = dot compEyev compNormalv
                    n = compN1 / compN2
                    sin2_t = n^2 * (1.0 - cos^2) in
                    if compN1 > compN2 && sin2_t > 1
                    then 1
                    else let cos_t = if compN1 > compN2
                                     then sqrt(1.0 - sin2_t)
                                     else cos
                             r0 = ((compN1 - compN2)/(compN1 +compN2))^2 in
                         r0 + (1 - r0) * (1-cos_t)^5


                                      


hit :: [Intersection] -> Maybe Intersection
hit [] = Nothing
hit list = let p = filter (\ (Intersection t s)  -> t > 0) list in
           if length p == 0
           then Nothing
           else Just $ head p

normal_at :: Shape -> Point -> Vector
normal_at s@(Sphere _ _ _) p =  let t = shapeTransform s
                                    object_point = (inverse t) `multMatVector` p
                                    object_normal = object_point `subP` makePoint 0 0 0
                                    (x, y, z, _) = transpose (inverse t) `multMatVector` object_normal
                                    world_normal = makeVector x y z in
                                normalize world_normal
normal_at s@(Plane _ _ _) p = let t = shapeTransform s in
                              makeVector 0 1 0
normal_at s@(Cube _ _ _) p@(x,y,z,_) = let maxc = maximum [abs x, abs y, abs z] in
                                       if maxc == abs x
                                       then makeVector x 0 0
                                       else if maxc == abs y
                                       then makeVector 0 y 0
                                       else makeVector 0 0 z
normal_at s@(Cylinder _ _ _ _ _ _) p@(x,y,z,_) = let dist = x^2 + z^2 in
                                                 if dist < 1 && y >= ((shapeMaximum s) - epsilon)
                                                 then makeVector 0 (1) 0
                                                 else if dist < 1 && y <= ((shapeMinimum s) + epsilon)
                                                 then makeVector 0 (-1) 0
                                                 else makeVector x 0 z

reflect :: Vector -> Vector -> Vector
reflect inward normal = let d = dot inward normal in
                        inward `subP` (normal `multVector` 2 `multVector` d)

color_at :: World -> Ray -> Int -> Color
color_at w r remaining =let i = intersect_world w r
                            h = hit i in
                        case h of
                          Nothing -> black
                          Just x -> let c = prepare_computations x r [] in
                                    shade_hit w c remaining
pattern_at_object :: Pattern -> Shape -> Point -> Color
pattern_at_object patt shape pt = let  shapeTrx = shapeTransform shape
                                       pattTrx = patternTransform patt
                                       object_point = (inverse shapeTrx) `multMatVector` pt
                                       pattern_point = (inverse pattTrx) `multMatVector` object_point in
                                       pattern_at patt pattern_point

pattern_at :: Pattern -> Point -> Color
pattern_at (GradientPattern c1 c2 _) (x, _, _, _) = let x' = (floor x)
                                                        distance = c2 `subColor` c1
                                                        frac = x - (fromIntegral x') in
                                                    c1 `addColor` (distance `multColorScalar` frac)

pattern_at (StripePattern c1 c2 _) (x, _, _, _) = if (floor x) `mod` 2 == 0
                                                  then c1
                                                  else c2

pattern_at (RingPattern c1 c2 _) (x, _, z, _) = if ((floor( sqrt(x^2 + z^2) ) ) `mod` 2) == 0
                                                then c1
                                                else c2

pattern_at (CheckersPattern c1 c2 _) (x,y,z,_) = if ((floor x) + (floor y) + (floor z)) `mod` 2 == 0
                                                 then c1
                                                 else c2
pattern_at (TestPattern _ _ _) (x,y,z,_) = (x,y,z)


reflected_color :: World -> Comps -> Int -> Color
reflected_color world comps remaining = let (Comp _ _ _ _ _ _ compOP _ compReflect _ _) = comps
                                            ref = materialReflective (shapeMaterial (compObject comps)) in
                                        if ref == 0 || remaining <= 0
                                        then black
                                        else let reflect_ray = Ray compOP compReflect
                                                 color = color_at world reflect_ray (remaining-1) in
                                        color `multColorScalar` ref

refracted_color :: World -> Comps -> Int -> Color
refracted_color world comps remaining = let (Comp _ compOb _ compEyev compNormalv _ compOP compUP compReflect compN1 compN2) = comps
                                            ref = materialTransparent (shapeMaterial (compObject comps))
                                            n_ratio = compN1/compN2
                                            cos_i = dot compEyev compNormalv --vector type
                                            sin2_t = ((1 - (cos_i^2)) ) * (n_ratio^2) in
                                        if ref == 0 || remaining <= 0 || sin2_t > 1
                                        then black
                                        else let cos_t = sqrt (1.0 - sin2_t)
                                                 direction = (compNormalv `multVector` (n_ratio * cos_i - cos_t)) `subP` (compEyev `multVector` n_ratio)
                                                 refract_ray = Ray compUP direction in
                                             (color_at world refract_ray (remaining - 1)) `multColorScalar` (materialTransparent (shapeMaterial compOb))


lighting :: Material -> Shape -> PointLight -> Point -> Vector -> Vector -> Bool -> Color
lighting m shape light point eyev normalv in_shadow = let Material  pattern color amb diff spec shine reflective transparent refractive = m
                                                          color' = case pattern of
                                                                    NoPattern _ _ _ -> color
                                                                    _ -> pattern_at_object pattern shape point
                                                          Light lightPosition intensity = light
                                                          effective_color = color' `multColor` intensity
                                                          lightv =  normalize $ lightPosition `subP` point
                                                          ambient = effective_color `multColorScalar` amb
                                                          light_dot_normal = dot lightv normalv in
                                                          if in_shadow
                                                          then ambient
                                                          else let diffuse = if light_dot_normal < 0
                                                                             then black
                                                                             else  effective_color `multColorScalar` diff `multColorScalar` light_dot_normal
                                                                   specular = if light_dot_normal < 0
                                                                              then black
                                                                              else let reflectv = reflect (negateV lightv) normalv
                                                                                       reflect_dot_eye = dot reflectv eyev in
                                                                                   if reflect_dot_eye <= 0
                                                                                   then black
                                                                                   else let factor = reflect_dot_eye ** shine in
                                                                                            intensity `multColorScalar` spec `multColorScalar` factor in
                                                                   ambient `addColor` diffuse `addColor` specular

is_shadowed :: World -> Point -> Bool
is_shadowed world@(World _ (Light lightPoint _)) point = let v = lightPoint `subP` point
                                                             distance = magnitude v
                                                             direction = normalize v
                                                             r = Ray point direction
                                                             intersections = intersect_world world r
                                                             h = hit intersections in
                                                         case h of
                                                         Nothing -> False
                                                         Just (Intersection t _) -> t < distance




view_transform :: Point -> Point -> Vector -> Matrix
view_transform from to up = let forward@(fx, fy, fz ,_) = normalize(to `subP` from)
                                upn = normalize up
                                (fromX, fromY, fromZ, _) = from
                                left@(lx, ly, lz, _) = cross forward upn
                                (tx,ty,tz, _) = cross left forward
                                orientation = Matrix 4 4 (V.fromList  [lx,    ly,    lz,  0,
                                                                       tx,    ty,    tz,  0,
                                                                     (-fx), (-fy), (-fz), 0,
                                                                       0,     0,     0,   1]) in
                            orientation `multMat` translation (-fromX) (-fromY) (-fromZ)

ray_for_pixel :: Camera -> Double -> Double -> Ray
ray_for_pixel camera px py = let xoffset = (px + 0.5) * camPixelSize camera
                                 yoffset = (py + 0.5) * camPixelSize camera
                                 world_x = (camHalfWidth camera) - xoffset
                                 world_y = (camHalfHeight camera) - yoffset
                                 pixel = (inverse $ camTransform camera) `multMatVector` (makePoint world_x world_y (-1))
                                 origin = (inverse $ camTransform camera) `multMatVector` (makePoint 0 0 0)
                                 direction = normalize (pixel `subP` origin) in
                             Ray origin direction

render :: Camera -> World -> Canvas
render cam w = let image = makeCanvas (camHsize cam) (camVsize cam)
                   colorXY = concatMap (\y -> map (\x -> let ray = ray_for_pixel cam (fromIntegral x) (fromIntegral y)
                                                             color = color_at w ray recursiveDepth in
                                                             ((x, y), color) ) [0..((camHsize cam)- 1)] ) [0..((camVsize cam)- 1)] in
               updateCanvas image colorXY



drawClock = do
            let rotation = rotation_z ((pi/6))
            let six = makePoint 0 1 0
            let seven = rotation `multMatVector` six
            let eight = rotation `multMatVector` seven
            let xs = foldl (\lst m -> if length lst == 0
                                      then [makePoint 0 1 0]
                                      else let h = head lst
                                               r = rotation `multMatVector` h in
                                               r:lst) []  [0..11]
--            let xs = [six, seven, eight]
            let c = makeCanvas 500 500
            let pointColors = map (\(x, y, z, _) -> ( (round ( (y * 150) + 250), round ((x * 150) + 250)), red) ) xs
            let updatedCanvas = updateCanvas c pointColors
            saveCanvasToFile "clock.ppm" updatedCanvas

circleCast = do
              -- start the ray at z = -5
              let ray_origin = makePoint 0 0 (-5)
              --put the wall at z = 10
              let wall_z = 10.0
              let wall_size = 7.0 :: Double
              --canvas size
              let canvas_pixels = 100
              let pixel_size = wall_size / canvas_pixels
              let halfCanvas = (wall_size/2)
              let canvas = makeCanvas 100 100
              let s = makeSphere
              let s' = s {shapeTransform = combTrans $ [scaling 1 0.5 1, shearing 1 0 0 0 0 0] }
              let pointColors = concatMap (\y -> let world_y = halfCanvas - pixel_size* y in
                                               map (\x -> let world_x = -halfCanvas + pixel_size* x
                                                              position = makePoint world_x world_y wall_z
                                                              r = Ray ray_origin (normalize (position `subP` ray_origin))
                                                              xs = intersect s r
                                                              h = hit xs in
                                                              case h of
                                                                Nothing -> ((round x, round y),black)
                                                                Just _ -> ((round x, round y),(1,0.2,1))
                                                   ) [0..(canvas_pixels-1)]
                                                              ) [0..(canvas_pixels-1)]
--              print pointColors

              let updatedCanvas = updateCanvas canvas pointColors
              saveCanvasToFile "circle.ppm" updatedCanvas

sphereCast = do
              let l = Light (makePoint (-10) 10 (-10)) (1, 1, 1)
              -- start the ray at z = -5
              let ray_origin = makePoint 0 0 (-5)
              --put the wall at z = 10
              let wall_z = 10.0
              let wall_size = 7.0 :: Double
              --canvas size
              let canvas_pixels = 500
              let pixel_size = wall_size / canvas_pixels
              let halfCanvas = (wall_size/2)
              let canvas = makeCanvas 500 500
              let s = makeSphere
--              let s' = s {shapeTransform = combTrans $ [scaling 1 0.5 1, shearing 1 0 0 0 0 0] }
              let s' = s {shapeMaterial = material {materialColor = (1, 0.2, 1), materialPattern = CheckersPattern blue (0.2, 0.8, 0.2) (combTrans[(scaling 1.5 1 1),(rotation_y(pi/4))]) }}
              let pointColors = concatMap (\y -> let world_y = halfCanvas - pixel_size* y in
                                               map (\x -> let world_x = -halfCanvas + pixel_size* x
                                                              position = makePoint world_x world_y wall_z
                                                              r@(Ray orig dir) = Ray ray_origin (normalize (position `subP` ray_origin))
                                                              xs = intersect s' r
                                                              h = hit xs in
                                                              case h of
                                                                Nothing -> ((round x, round y),black)
                                                                Just (Intersection tValue shape) -> let point = rayPosition r tValue
                                                                                                        normal = normal_at shape point
                                                                                                        eye = negateV dir
                                                                                                        color = lighting (shapeMaterial shape) shape l point eye normal False in
                                                                                                    ((round x, round y), color)
                                                   ) [0..(canvas_pixels-1)]
                                                              ) [0..(canvas_pixels-1)]
--              print pointColors

              let updatedCanvas = updateCanvas canvas pointColors
              saveCanvasToFile "sphere.ppm" updatedCanvas

roomCast= do
            let floor = makeSphere
            let floor' = floor {shapeTransform = scaling 10 0.01 10 , shapeMaterial = material { materialColor = (1, 0.9, 0.9), materialSpecular = 0, materialReflective = 0.5} }
            let wall' = floor {shapeTransform = scaling 10 0.01 10 , shapeMaterial = material { materialColor = (1, 0.9, 0.9), materialSpecular = 0} }



            let left_wall = makeSphere
            let left_wall' = left_wall {shapeTransform = combTrans [ (translation 0 0 5), (rotation_y (-pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
             shapeMaterial =  shapeMaterial wall'}
            let right_wall = makeSphere
            let right_wall' = right_wall {shapeTransform = combTrans [(translation 0 0 5), (rotation_y (pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
             shapeMaterial =  shapeMaterial wall'}

--            let right_wall = makeSphere
--            let right_wall' = right_wall {shapeTransform = combTrans [(translation 0 0 5), (rotation_y (pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
--             shapeMaterial =  shapeMaterial floor'}

            let middleSphere = makeSphere
            let middleSphere' = middleSphere {shapeTransform = (translation (-0.5) 1 0.5),
                         shapeMaterial = material { materialColor = (0.1, 1, 0.5), materialDiffuse = 0.7, materialSpecular = 0.3}}
            let rightSphere = makeSphere
            let rightSphere' = rightSphere {shapeTransform = (translation 1.5 0.5 (-0.5)) `multMat` scaling 0.5 0.5 0.5 ,
                         shapeMaterial = material { materialColor = (0.5, 1, 0.1), materialDiffuse = 0.7, materialSpecular = 0.3 }}
            let leftSphere = makeSphere
            let leftSphere' = leftSphere {shapeTransform = (translation (-1.5) 0.33 (-0.75)) `multMat` scaling 0.33 0.33 0.33 ,
                         shapeMaterial = material { materialColor = (0.2, 0.8, 0.2), materialDiffuse = 0.7, materialSpecular = 0.3 }}

            let world = defaultWorld
            let world' = world {worldLight = Light (makePoint (-10) 10 (-10)) (1, 1, 1), worldObjects = [floor', left_wall', right_wall', middleSphere', rightSphere',leftSphere']}
                                                                                                        --floor', left_wall', right_wall',

            let camera = makeCamera 400 200 (pi/3)
            let camera' = camera {camTransform = view_transform (makePoint 0 1.5 (-5)) (makePoint 0 1 0) (makeVector 0 1 0)}

            let canvas = render camera' world'

            saveCanvasToFile "roomCast.ppm" canvas


planesCast= do
            let floor = makePlane
            let floor' = floor {shapeTransform = scaling 10 0.01 10 , shapeMaterial = material { materialColor = (1, 0.9, 0.9),
              materialSpecular = 0, materialReflective = 0.3, materialPattern = CheckersPattern white black (scaling 0.2 0.2 0.2)} }
            let wall' = floor {shapeTransform = scaling 10 0.01 10 , shapeMaterial = material { materialColor = (0.25, 1, 0.25), materialSpecular = 0} }



            let left_wall = makePlane
            let left_wall' = left_wall {shapeTransform = combTrans [ (translation 0 0 5), (rotation_y (-pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
             shapeMaterial =  shapeMaterial wall'}
            let right_wall = makePlane
            let right_wall' = right_wall {shapeTransform = combTrans [(translation 0 0 5), (rotation_y (pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
             shapeMaterial =  shapeMaterial wall'}

--            let right_wall = makeSphere
--            let right_wall' = right_wall {shapeTransform = combTrans [(translation 0 0 5), (rotation_y (pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
--             shapeMaterial =  shapeMaterial floor'}

            let middleSphere = makeSphere
            let middleSphere' = middleSphere {shapeTransform = (translation (-0.5) 1 0.5),
                         shapeMaterial = material { materialColor = (0.09, 0.09, 0.9), materialDiffuse = 0.5,
                         materialAmbient = 0.3, materialSpecular = 1, materialReflective = 0.9,
                         materialTransparent = 1, materialShininess = 300, materialPattern = GradientPattern white black (scaling 2 2 2) } }
            let backSphere = makeSphere
            let backSphere' = backSphere {shapeTransform = (translation (-0.5) 1 4) `multMat` scaling 0.5 0.5 0.5,
                              shapeMaterial = material { materialColor = (0.1, 1, 0.5), materialDiffuse = 0.7,
                              materialSpecular = 0.3, materialShininess = 400 } }
            let rightSphere = makeSphere
            let rightSphere' = rightSphere {shapeTransform = (translation 1.5 0.5 (-0.5)) `multMat` scaling 0.5 0.5 0.5 ,
                         shapeMaterial = material { materialColor = (0.5, 1, 0.1), materialDiffuse = 0.7,
                         materialSpecular = 0.3, materialReflective = 0.1, materialShininess = 500,
                         materialPattern = StripePattern red (0,0,1) (scaling 0.2 0.2 0.2)} }
            let leftSphere = makeSphere
            let leftSphere' = leftSphere {shapeTransform = (translation (-1.5) 0.33 (-0.75)) `multMat` scaling 0.33 0.33 0.33 ,
                         shapeMaterial = material { materialColor = (1, 0.8, 0.1), materialDiffuse = 0.4,
                         materialSpecular = 0.3, materialReflective = 0.5, materialAmbient = 0.4, materialShininess = 200} }

            let world = defaultWorld
            let world' = world {worldLight = Light (makePoint (-10) 10 (-10)) (1, 1, 1), worldObjects = [floor', middleSphere', backSphere', rightSphere',leftSphere']}
                                                                                                        --floor', left_wall', right_wall',

            let camera = makeCamera 400 200 (pi/3)
            let camera' = camera {camTransform = view_transform (makePoint 0 1.5 (-5)) (makePoint 0 1 0) (makeVector 0 1 0)}

            let canvas = render camera' world'

            saveCanvasToFile "planesCast.ppm" canvas

cubeCast =  do
            let floor = makeSphere
            let floor' = floor {shapeTransform = scaling 10 0.01 10 , shapeMaterial = material { materialColor = (1, 0.9, 0.9), materialSpecular = 0, materialReflective = 0.5} }
            let wall' = floor {shapeTransform = scaling 10 0.01 10 , shapeMaterial = material { materialColor = (1, 0.9, 0.9), materialSpecular = 0} }



            let left_wall = makeSphere
            let left_wall' = left_wall {shapeTransform = combTrans [ (translation 0 0 5), (rotation_y (-pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
             shapeMaterial =  shapeMaterial wall'}
            let right_wall = makeSphere
            let right_wall' = right_wall {shapeTransform = combTrans [(translation 0 0 5), (rotation_y (pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
             shapeMaterial =  shapeMaterial wall'}

--            let right_wall = makeSphere
--            let right_wall' = right_wall {shapeTransform = combTrans [(translation 0 0 5), (rotation_y (pi/4)), (rotation_x (pi/2)), (scaling 10 0.01 10)],
--             shapeMaterial =  shapeMaterial floor'}

            let middleCube = makeCube
            let middleCube' = middleCube {shapeTransform = (translation (-0.5) 1 0.5),
                         shapeMaterial = material { materialColor = (0.1, 1, 0.5), materialDiffuse = 0.7, materialSpecular = 0.3}}
            let rightCube = makeCube
            let rightCube' = rightCube {shapeTransform = (translation 1.5 0.5 (-0.5)) `multMat` scaling 0.5 0.5 0.5 ,
                         shapeMaterial = material { materialColor = (0.5, 1, 0.1), materialDiffuse = 0.7, materialSpecular = 0.3 }}
            let leftCube = makeCube
            let leftCube' = leftCube {shapeTransform = (translation (-1.5) 0.33 (-0.75)) `multMat` scaling 0.33 0.33 0.33 ,
                         shapeMaterial = material { materialColor = (0.2, 0.8, 0.2), materialDiffuse = 0.7, materialSpecular = 0.3 }}

            let world = defaultWorld
            let world' = world {worldLight = Light (makePoint (-10) 10 (-10)) (1, 1, 1), worldObjects = [floor', left_wall', right_wall', middleCube', rightCube',leftCube']}
                                                                                                        --floor', left_wall', right_wall',

            let camera = makeCamera 400 200 (pi/3)
            let camera' = camera {camTransform = view_transform (makePoint 0 1.5 (-5)) (makePoint 0 1 0) (makeVector 0 1 0)}

            let canvas = render camera' world'

            saveCanvasToFile "cubeCast.ppm" canvas




module Main where

import Data.Vector
import System.Console.Terminal.Size
import Data.Bifunctor
import Data.Ix
import Data.List.Split

type NormalVector = Vector

type Luminance = Float

type ShapePoint = (Vector Float, NormalVector Float)

type ShadedPoint = (Vector Float, Luminance)

type ScreenPos = (Int, Int)

type RenderablePoint = (ScreenPos, Char)

type Grid = [[Char]]

k :: Float 
k = 18
k2 :: Float 
k2 = 6.5

-- | A torus is essentially a circle cloned around a central point;
-- we thus define r as being the radius of the circle,
r :: Float
r = 1.2
-- and r2 a being the distance between the center of the 
-- circle and the central point of the torus.
r2 :: Float
r2 = 2.5

-- | Generates every point of a circle of radius r and
-- center c.
generateCircle :: [ShapePoint]
generateCircle = map generatePoint [0.0,0.1..2*pi]
  where
    generatePoint :: Float -> ShapePoint
    generatePoint theta = (Vector r2 0 0 + vradius theta, vnormal theta)

    -- | Generates a vector corresponding to the
    -- radius, rotated by the given angle theta.
    -- It is centered at point (0,0,0); we need to add that
    -- vector the vector c.
    vradius :: Float -> Vector Float
    vradius theta = Vector (r * cos theta) (r * sin theta) 0

    -- | The surface normal of one of the point of the circle is
    -- basically the same as a point on a unit circle centered at (0,0,0).
    -- We do that to make computations easier.
    vnormal :: Float -> NormalVector Float
    vnormal theta = Vector (cos theta) (sin theta) 0

-- | Generates every point at the surface of a torus of center c,
-- rotated on the x-axis by A and on the y-axis by B.
generateTorus :: Float -> Float -> [ShadedPoint]
generateTorus a b = concatMap circle [0.0,0.1..2*pi]
  where
    -- | Precomputes some values.
    sin_A = sin a
    sin_B = sin b
    cos_A = cos a
    cos_B = cos b

    -- | Calculate one of the "slice" of the torus,
    -- by rotating a circle by A and B.
    circle :: Float -> [ShadedPoint]
    circle phi = map (luminance . bimap rotate rotate) generateCircle
      where
        sin_phi = sin phi
        cos_phi = cos phi

        -- | Applies a rotation matrix to a point.
        rotate :: Vector Float -> Vector Float
        rotate (Vector x y z) = Vector
          ((x * (cos_B * cos_phi + sin_A * sin_B * sin_phi)) - y * cos_A * cos_B)
          ((x * (sin_B * cos_phi - cos_A * sin_B * sin_phi)) + y * cos_A * cos_B)
          (x * cos_A * sin_phi + y * sin_A)

-- | Computes the luminance of a point based of its normal vector,
-- against the light vector (0,1,-1).
luminance :: ShapePoint -> ShadedPoint
luminance (point, Vector nx ny nz) = (point, nx + ny + nz)

luminanceToChar :: Luminance -> Char
luminanceToChar l
  | l > 0     = chars !! min (floor (l*8)) 11 --chars !! index
  | otherwise = ' '
  where 
    sqrt2 = 1.4142135624
    chars = ".,-~:;=!*#$@"
    maxIndex = fromIntegral (length chars - 1)
    index = floor $ l * maxIndex / sqrt2

zbuffer :: [(Vector Float, RenderablePoint)] -> [RenderablePoint]
zbuffer coordinates = map (solve . conflicts) $ [ (x, y) | y <- take 24 [0..], x <- take 40 [0..] ] --((0,0), (80,24))
  where
    conflicts :: ScreenPos -> [(Vector Float, RenderablePoint)]
    conflicts pos
      | null filtered = [(Vector 0 0 0, (pos, ' '))]
      | otherwise     = filtered
      where filtered = filter ((== pos) . fst . snd) coordinates
      
    solve :: [(Vector Float, RenderablePoint)] -> RenderablePoint
    solve = snd . foldl1 vmax
      where 
        vmax v@(Vector x y z,_) v'@(Vector _ _ maxZ,_)
          | z > maxZ  = v
          | otherwise = v'

-- | Projects a vector to a point on the screen.
project :: Vector Float -> ScreenPos
project v@(Vector _ _ z) = pair . fmap (round . (* (k/(k2 + z)))) $ v
  where pair (Vector x y _) = (x+20, y+10)

-- | Creates our final render made out of char,
-- based on screen coordinates that must get rendered.
-- Checkout: https://stackoverflow.com/questions/23080908/printing-2d-grid-from-list-of-triples
renderScreenCoordinates :: [RenderablePoint] -> Grid
renderScreenCoordinates = chunksOf 80 . concatMap ((\c -> c:[c]) . snd)

-- | Starts the rendering process.
render :: Float -> Float -> [[Char]]
render a b = do
  let torus = generateTorus a b
  let renderable = map (\(pos, lum) -> (pos, (project pos, luminanceToChar lum))) torus :: [(Vector Float, RenderablePoint)]
  let zbuffered = zbuffer renderable

  renderScreenCoordinates zbuffered

renderLoop :: Float -> Float -> IO ()
renderLoop a b = do
  putStr "\x1b[H"
  putStr . init . unlines $ render a b
  renderLoop (a+0.1) (b-0.1)

main :: IO ()
main = do
  window <- size
  window <- case window of
    Just a -> return a
    Nothing -> return $ Window 80 24

  renderLoop (2/pi) 0

module Main where

import Data.Vector
import System.Console.Terminal.Size

type ScreenPos = (Int, Int)

type Grid = [[Char]]

k :: Float 
k = 15
k2 :: Float 
k2 = 5

-- | A torus is essentially a circle cloned around a central point;
-- we thus define r as being the radius of the circle,
r :: Float
r = 1
-- and r2 a being the distance between the center of the 
-- circle and the central point of the torus.
r2 :: Float
r2 = 2

-- | Generates every point of a circle of radius r and
-- center c.
generateCircle :: [Vector Float]
generateCircle = map ((+) (Vector r2 0 0) . vradius) [0.0,0.2..2*pi] 
  where
    -- | Generates a vector corresponding to the
    -- radius, rotated by the given angle theta.
    -- It is centered at point (0,0,0); we need to add that
    -- vector the vector c.
    vradius :: Float -> Vector Float
    vradius theta = Vector (r * cos theta) (r * sin theta) 0

-- | Generates every point at the surface of a torus of center c,
-- rotated on the x-axis by A and on the y-axis by B.
generateTorus :: Float -> Float -> [Vector Float]
generateTorus a b = concatMap (circle a b) [0.0,0.1..2*pi]
  where
    -- | Precomputes some values.
    sin_A = sin a
    sin_B = sin b
    cos_A = cos a
    cos_B = cos b

    -- | Calculate one of the "slice" of the torus,
    -- by rotating a circle by A and B.
    circle :: Float -> Float -> Float -> [Vector Float]
    circle a b phi = map (rotate phi) generateCircle
      where
        sin_phi = sin phi
        cos_phi = cos phi

        -- | Applies a rotation matrix to a point.
        rotate phi (Vector x y z) = Vector
          ((x * (cos_B * cos_phi + sin_A * sin_B * sin_phi)) - y * cos_A * cos_B)
          ((x * (sin_B * cos_phi - cos_A * sin_B * sin_phi)) + y * cos_A * cos_B)
          (x * cos_A * sin_phi + y * sin_A)

-- | Projects a vector to a point on the screen.
project :: Vector Float -> ScreenPos
project v@(Vector _ _ z) = pair . fmap (round . (* (k/(k2 + z)))) $ v
  where pair (Vector x y _) = (x, y)

-- | Creates our final render made out of char,
-- based on screen coordinates that must get rendered.
renderScreenCoordinates :: [ScreenPos] -> Grid
renderScreenCoordinates coordinates = 
  map (concatMap (\cell -> if mustRender cell then ".." else "  ")) gridCoordinates
  where 
    gridCoordinates :: [[ScreenPos]]
    gridCoordinates = [ 
      [ (x, y) | x <- take 40 [0..] ]
      | y <- take 24 [0..] ]

    mustRender :: ScreenPos -> Bool
    mustRender (x,y) = (x-20, y-12) `elem` coordinates

-- | Starts the rendering process.
render :: Window Int -> [[Char]]
render win@(Window w h) = coordinates
  where 
    coordinates = renderScreenCoordinates $ 
      map project $ 
      generateTorus (pi/2) (pi/2)

main :: IO ()
main = do
  window <- size
  window <- case window of
    Just a -> return a
    Nothing -> return $ Window 80 24

  let screen = unlines $ render window
  putStr screen

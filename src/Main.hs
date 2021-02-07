module Main where

import Data.Vector
import System.Console.Terminal.Size

type ScreenPos = (Int, Int)

type Grid = [[Char]]

k :: Float 
k = 1

-- | A torus is essentially a circle cloned around a central point;
-- we thus define r as being the radius of the center,
r :: Float
r = 5
-- and r' a being the distance between the center of the 
-- circle and the central point of the torus.
r' :: Float
r' = 25

-- | Generates every point of a circle of radius r and
-- center c.
generateCircle :: Vector Float -> [Vector Float]
generateCircle c = map ((+) c . vradius) [0.0,0.1..2*pi] 
  where
    -- | Generates a vector corresponding to the
    -- radius, rotated by the given angle theta.
    -- It is centered at point (0,0,0); we need to add that
    -- vector the vector c.
    vradius :: Float -> Vector Float
    vradius theta = Vector (r * cos theta) (r * sin theta) 0

-- | Generates every point at the surface of a torus of center c.
generateTorus :: Vector Float -> [Vector Float]
generateTorus c = concatMap circle [0.0,0.05..2*pi]
  where
    circle phi = map (rotateCircle phi) . generateCircle $ circleCenter
      where
        circleCenter = c + Vector r' 0 0
        -- | Applies a rotation matrix to a point.
        rotateCircle phi (Vector x y z) = Vector (x * cos phi) y (negate $ sin phi * (r' + r*cos phi))

-- | Projects a vector to a point on the screen
project :: Vector Float -> ScreenPos
project v@(Vector _ _ z) = pair . fmap (round . (* (k/z))) $ v
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
    mustRender pos = pos `elem` coordinates

-- | Starts the rendering process.
render :: Window Int -> [[Char]]
render win@(Window w h) = coordinates
  where 
    coordinates = renderScreenCoordinates $ map project $ generateTorus (Vector r' 0 1)

main :: IO ()
main = do
  window <- size
  window <- case window of
    Just a -> return a
    Nothing -> return $ Window 80 24

  let screen = unlines $ render window
  putStr screen

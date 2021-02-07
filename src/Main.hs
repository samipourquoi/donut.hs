module Main where

import Data.Vector

_r :: Float
_r = 1.0
_R :: Float
_R = 2.0

drawCircle :: Vector Float -> [Vector Float]
drawCircle center = map ((+) center . vradius) [0,0.1..2*pi] 
  where 
    vradius :: Float -> Vector Float
    vradius theta = Vector (_r * cos theta) (_r * sin theta) 0

render :: [[Char]]
render = ["hello", "world"]

main :: IO ()
main = do
  let screen = unlines render
  putStr screen

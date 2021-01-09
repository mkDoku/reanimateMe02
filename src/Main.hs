#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main ( main ) where

import           Reanimate
import           Reanimate.Builtin.Documentation

import           Linear.V2

main :: IO ()
main = reanimate
  $ docEnv
  $ staticFrame 1
  $ scale 3.5
    staticStar

staticStar :: SVG
staticStar =
  mkLinePathClosed coordsInCartesianU

fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180

fromPolar :: Floating a => a -> a -> V2 a
fromPolar r phi = V2 x y
  where
    x = r * sin( fromDegrees phi )
    y = r * cos( fromDegrees phi )

fromPolarU :: Floating a => a -> V2 a
fromPolarU = fromPolar 1

coordsInPolar :: [Double]
coordsInPolar = [0, 144, 288, 72, 216]

coordsInCartesianU :: [(Double, Double)]
coordsInCartesianU = fmap ( (\(V2 x y) -> (x, y)) . fromPolarU ) coordsInPolar

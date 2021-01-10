#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Transition

import           Linear.V2

import qualified Data.Text                       as T
import           Text.Printf


main :: IO ()
main = reanimate
  $ docEnv
  animationPolarCoordinates

-- |
-- phi
phi :: Double
phi = 50

-- |
-- Aspect ratio
ratio :: Double
ratio = 16/9


-----------------------------------------------------------------------------

-- |
-- Static animation to demonstrate polar coordiantes
animationPolarCoordinates :: Animation
animationPolarCoordinates =
  -- adjust viewport
    mapA (withViewBox (-0.5*ratio, -0.5, 1.5*ratio, 1.5))
  $ chainT parA
  -- Will only produce one frame. Awesome!
  -- (1/60) will work for mp4 but not gif
  $ staticFrame (3/60)
  <$> [ xAxis
      , yAxis
      , pointPolar phi
      , rVector    phi
      , phiShow    phi
      ]

-- |
-- Generate SVG of a point with text
-- of the coordinates (polar coordinates)
pointPolar :: Double -> SVG
pointPolar ang = translate x y $ mkGroup
 [
      withFillOpacity 1
    $ withFillColor "black"
    $ withStrokeColor "white"
    $ mkCircle 0.01
  ,
      translate 0.05 0
    $ withFillColor "black"
    $ withFillOpacity 1
    $ scaleToHeight 0.08
    $ latex txt
 ]
  where (V2 x y) = fromPolarU ang
        txt      =   T.pack
                   $ printf "($r$,$\\phi$) = (%1.1f,%2.0f\\degree)" (1.0 :: Double) ang

rVector :: Double -> SVG
rVector ang = mkGroup
 [ projectionLine (0,0) (x,y)
 ,
     translate (x/2-0.04) (y/2+0.04)
   $ withFillColor "black"
   $ withFillOpacity 1
   $ scaleToHeight 0.05
   $ latex txt
 ]
  where (V2 x y) = fromPolarU ang
        txt      =   T.pack
                   $ printf "$\\vec{r}$"

phiShow :: Double -> SVG
phiShow ang = mkGroup
 [  withStrokeWidth 0.002
  $ withStrokeColor "blue"
  $ phiCircle 100 ang
 ,
     translate 0.1 0.05
   $ withFillColor "black"
   $ withFillOpacity 1
   $ scaleToHeight 0.06
   $ latex
   $ T.pack
   $ printf "$\\phi$"
 ]

phiCircle :: Int -> Double -> SVG
phiCircle segments ang =
  mkLinePath $ trans $ (* (ang/segments')) <$> range
    where
      trans     = fmap ( (\(V2 x y) -> (x, y)) . fromPolar 0.2 )
      range     = fromIntegral <$> [0..segments]
      segments' = fromIntegral segments




-- |
-- Static animation to demonstrate Cartesian coordiantes
animationCartesianCoordinates :: Animation
animationCartesianCoordinates =
  -- adjust viewport
    mapA (withViewBox (-0.5*ratio, -0.5, 1.5*ratio, 1.5))
  $ chainT parA
  -- Will only produce one frame. Awesome!
  -- (1/60) will work for mp4 but not gif
  $ staticFrame (3/60)
  <$> [ xAxis
      , yAxis
      , point phi
      , projection X phi
      , projection Y phi
      , tic X phi
      , tic Y phi
      ]

-- |
-- Data type to distinguish between
-- x and y coordinates
data Dimension = X | Y

-- |
-- Generate SVG for x axis
xAxis :: SVG
xAxis = mkAxis X

-- |
-- Generate SVG for y axis
yAxis :: SVG
yAxis = mkAxis Y

-- |
-- Helper function to generate
-- an axis dependent on the 'Dimension'
mkAxis :: Dimension -> SVG
mkAxis X = customLine (-2,  0) (5, 0)
mkAxis Y = customLine (0 , -2) (0, 2)

-- |
-- Generate modified line
-- Dot-free notations seems a little bit "weird"
-- for two arguments
customLine :: (Double, Double) -> (Double, Double) -> SVG
customLine = (.) (withStrokeColor "black" . withStrokeWidth 0.004) . mkLine

-- |
-- Generate SVG of a point with text
-- of the coordinates (Cartesian)
point :: Double -> SVG
point ang = translate x y $ mkGroup
 [
      withFillOpacity 1
    $ withFillColor "black"
    $ withStrokeColor "white"
    $ mkCircle 0.01
  ,
      translate 0.05 0
    $ withFillColor "black"
    $ withFillOpacity 1
    $ scaleToHeight 0.08
    $ latex txt
 ]
  where (V2 x y) = fromPolarU ang
        txt      =   T.pack
                   $ printf "($x$,$y$) = (%1.3f,%1.3f)" x y

-- |
-- Indicate the projection of (x,y) to either
-- the x- or y-axis (Cartesian coordinates)
projection :: Dimension -> Double -> SVG
projection dim ang =
  case dim of
    X -> projectionLine (x, 0) (x, y)
    _ -> projectionLine (0, y) (x, y)
    where (V2 x y) = fromPolarU ang

-- |
-- Modified line for projection
projectionLine :: (Double, Double) -> (Double, Double) -> SVG
projectionLine = (.) ( withStrokeDashArray [0.0, 0.01, 0.0]
                     . withStrokeColor "blue"
                     . withStrokeWidth 0.002) . mkLine

--
tic :: Dimension -> Double -> SVG
tic dim ang =
  case dim of
    X -> ticLine (x, -ticklen) (x, ticklen)
    _ -> ticLine (-ticklen, y) (ticklen, y)
    where (V2 x y) = fromPolarU ang
          ticklen  = 0.01

ticLine :: (Double, Double) -> (Double, Double) -> SVG
ticLine = (.) (   withStrokeColor "black"
                 . withStrokeWidth 0.004) . mkLine

-----------------------------------------------------------------------------

animationStaticStar90 :: Animation
animationStaticStar90 =
    staticFrame (3/60) $ scale 3.5 staticStar90

animationStaticStar :: Animation
animationStaticStar =
    staticFrame (3/60) $ scale 3.5 staticStar

staticStar90 :: SVG
staticStar90 =
  mkLinePathClosed $ trans coordsInPolar
    where
      trans = fmap ( (\(V2 x y) -> (x, y)) . fromPolarU . (+90) )

-- |
-- Draw symmetric pentagram by using
-- Cartesian coordinates which are in the
-- set of the unit circle
staticStar :: SVG
staticStar =
  mkLinePathClosed coordsInCartesianU

-- |
-- Take the coordinates for the pentagram in
-- polar coordinates (0+72*n) with n = 0..5
-- and transform them into Cartesian coordinates
coordsInCartesianU :: [(Double, Double)]
coordsInCartesianU = fmap ( (\(V2 x y) -> (x, y)) . fromPolarU ) coordsInPolar

-- |
-- Polar coordinates on the unit circle
coordsInPolar :: [Double]
coordsInPolar = [0, 144, 288, 72, 216]

-- |
-- Transform polar coordinates to Cartesian coordinates (unit circle)
fromPolarU :: Floating a => a -> V2 a
fromPolarU = fromPolar 1

-- |
-- Transform polar coordinates to Cartesian coordinates (arbitrary length)
fromPolar :: Floating a => a -> a -> V2 a
fromPolar r ang = V2 x y
  where
    x = r * cos( fromDegrees ang )
    y = r * sin( fromDegrees ang )

-- |
-- Transform from degrees to radian
fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180

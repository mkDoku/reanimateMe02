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
  animationCreativeStar

-- |
-- phi
phi :: Double
phi = 50

-- |
-- Aspect ratio
ratio :: Double
ratio = 16/9


-----------------------------------------------------------------------------
-- First Part (Cartesian and polar coordinates)
-----------------------------------------------------------------------------

-- |
-- Static animation to demonstrate polar coordiantes
animationPolarCoordinates :: Animation
animationPolarCoordinates =
  -- adjust viewport
    mapA (withViewBox (-0.15*ratio, -0.15, 1.1*ratio, 1.1))
  $ chainT parA
  -- Will only produce one frame. Awesome!
  -- (1/60) will work for mp4 but not for gif
  $ staticFrame (3/60)
  <$> [ xAxis
      , yAxis
      , pointPolar phi
      , rVector    phi
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
    mapA (withViewBox (-0.15*ratio, -0.15, 1.1*ratio, 1.1))
--    mapA (withViewBox (-0.5*ratio, -0.5, 1.5*ratio, 1.5))
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

animationIncorrectStaticStar :: Animation
animationIncorrectStaticStar =
    staticFrame (3/60)
  $ withStrokeWidth  0.01
  $ mkLinePathClosed coordsCartesian
  where
    coordsCartesian =
      transformCoord . fromPolarU <$> coordsInPolar
    transformCoord (V2 x y) = (x, y)

animationStaticStar90 :: Animation
animationStaticStar90 =
    mapA (withViewBox (-1.5*ratio,  -1.5, 3*ratio, 3))
    $ staticFrame (3/60) $ staticStar90

animationStaticStar :: Animation
animationStaticStar =
    staticFrame (3/60) $ staticStar

animationStaticStarView :: Animation
animationStaticStarView =
    mapA (withViewBox (-1.5*ratio,  -1.5, 3*ratio, 3))
  $ staticFrame (3/60)
  $ staticStar

staticStar90 :: SVG
staticStar90 =
    withStrokeWidth 0.01
  $ mkLinePathClosed coordsCartesianRearranged90
    where
      coordsCartesianRearranged90 =
        transformCoord . fromPolarU . (+90) <$> coordsInPolarRearranged
      transformCoord (V2 x y) = (x, y)

-- |
-- Draw symmetric pentagram by using
-- Cartesian coordinates which are in the
-- set of the unit circle
staticStar :: SVG
staticStar =
    withStrokeWidth  0.01
  $ mkLinePathClosed coordsCartesianRearranged
  where
    coordsCartesianRearranged =
      transformCoord . fromPolarU <$> coordsInPolarRearranged
    transformCoord (V2 x y) = (x, y)

-- |
-- Take the coordinates for the pentagram in
-- polar coordinates (0+72*n) with n = 0..5
-- and transform them into Cartesian coordinates
coordsInCartesianU :: [(Double, Double)]
coordsInCartesianU = fmap ( (\(V2 x y) -> (x, y)) . fromPolarU ) coordsInPolarRearranged

-- |
-- Polar coordinates on the unit circle
coordsInPolar :: [Double]
coordsInPolar = [0, 72, 144, 216, 288]

coordsInPolarRearranged :: [Double]
coordsInPolarRearranged = [0, 144, 288, 72, 216]

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

-----------------------------------------------------------------------------
-- Second Part (`Scene` and `Var`)
-----------------------------------------------------------------------------

animationStaticDot :: Animation
animationStaticDot =
    mapA (withViewBox (-1.5*ratio,  -1.5, 3*ratio, 3))
  $ setDuration (3/60)
  $ scene firstScene

animationJumpingDot :: Animation
animationJumpingDot =
    mapA (withViewBox (-1.5*ratio,  -1.5, 3*ratio, 3))
  $ scene secondScene

animationSmoothDot :: Animation
animationSmoothDot =
    mapA (withViewBox (-1.5*ratio,  -1.5, 3*ratio, 3))
  $ scene thirdScene

animationCreativeStar :: Animation
animationCreativeStar =
    mapA (withViewBox (-1.5*ratio,  -1.5, 3*ratio, 3))
  $ scene fourthScene

firstScene :: Scene s ()
firstScene = do
  newSpriteSVG_ $ dot (fromPolarU 0)

secondScene :: Scene s ()
secondScene = do
  currentPosition <- newVar $ fromPolarU 0
  newSprite_ $ dot <$> unVar currentPosition

  wait 1

  let moveDot newPos = do
      writeVar currentPosition newPos
      wait 1

  moveDot (fromPolarU 72)
  moveDot (fromPolarU 144)
  moveDot (fromPolarU 216)
  moveDot (fromPolarU 288)

thirdScene :: Scene s ()
thirdScene = do
  currentPosition <- newVar $ fromPolarU 0
  newSprite_ $ dot <$> unVar currentPosition

  wait 1

  let moveDot (V2 x' y') = do
      -- read and store current position
      -- before modifying it
      (V2 x y) <- readVar currentPosition
      -- calculate dx and dy, so the coordinate difference
      -- between start and end coordinate
      --
      -- this is handy, because the `t` value later on
      -- ranges from 0 to 1
      let (deltaX, deltaY) = (x'-x, y'-y)
      -- modify the value of the current position over
      -- a duration of 2 seconds (120 Frames)
      -- This modification will add the differences dx and dy
      -- incrementally over time => smooth movement
      tweenVar currentPosition 2
        $ \_ t -> V2 (x+deltaX*t) (y+deltaY*t)
      wait 1

  moveDot (fromPolarU 72)
  moveDot (fromPolarU 144)
  moveDot (fromPolarU 216)
  moveDot (fromPolarU 288)
  moveDot (fromPolarU 360)

dot :: V2 Double -> SVG
dot (V2 x y) =
    translate x y
  $ withFillOpacity 1
  $ withFillColor "blue"
  $ withStrokeColor "white"
  $ mkCircle 0.02


-- |
-- Modification of `thirdScene` to use `staticStar90`
-- and some "creative" changes
--
-- Polar coordinate, i.e. the angle, are used as variable
-- instead of Cartesian coordinate in `thirdScene`
fourthScene :: Scene s ()
fourthScene = do
  currentAngle <- newVar 0
  newSprite_ $ rotatedStar <$> unVar currentAngle

  let moveSprite ang' = do
      ang <- readVar currentAngle
      let deltaAng = ang'-ang
      tweenVar currentAngle 2
        $ \_ t -> ang+deltaAng*t

  moveSprite  72
  moveSprite 144
  moveSprite 216
  moveSprite 288
  moveSprite 360


-- |
-- Added changing color
-- Added rotation around the center
-- Added weird scaling behavior
--
-- to `staticStar90`
rotatedStar :: Double -> SVG
rotatedStar ang = translate x y
  $ rotateAroundCenter (ang*4)
  $ scale (fromInteger ( rem (round ang) 72) / 72)
  $ withFillOpacity 1
  $ withFillColor (chooseFillColor ang)
  $ scale 0.5 staticStar90
  where
    (V2 x y) = fromPolarU ang

-- |
-- Change color of star according to it's position
chooseFillColor :: Double -> String
chooseFillColor ang
 | ang <  72 = "blue"
 | ang < 144 = "yellow"
 | ang < 218 = "green"
 | ang < 288 = "red"
 | otherwise = "olive"

# Repo for second blog post

## How to use

Change the `Main.hs` file according to the animation you want to compile.

Then compile the animation, e.g. as gif:

```bash
stack ./src/Main.hs -- render -w 3840 -h 2160 --compile --format gif
```

## Animations

### Static star (incorrect)

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationIncorrectStaticStar
```

![A incorrect static star](./images/incorrect.gif)

### Static star

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationStaticStar
```

![A static star](./images/staticStar.gif)

### Static star (changed view)

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationStaticStarView
```

![A static star with changed view](./images/staticStarView.gif)

### Static star (rotated by 90°)

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationStaticStar90
```

![A static star rotated by 90 degrees](./images/staticStar90.gif)

### Demo Cartesian coordinates

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationCartesianCoordinates
```

![Coordinates of point in Cartesian coordinates](./images/PointCartesian.gif)


### Demo polar coordinates

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationPolarCoordinates
```

![Coordinates of point in polar coordinates](./images/PointPolar.gif)


### Static dot

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationStaticDot
```

![A static blue dot](./images/staticDot.gif)

### Jumping dot

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationJumpingDot
```

![A jumping blue dot](./images/jumpingDot.gif)

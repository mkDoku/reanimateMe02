# Repo for second blog post

## How to use

Change the `Main.hs` file according to the animation you want to compile.

Then compile the animation, e.g. as gif:

```bash
stack ./src/Main.hs -- render -w 3840 -h 2160 --compile --format gif
```

## Animations

### Static star

```haskell
main :: IO ()
main = reanimate
  $ docEnv
  animationStaticStar
```

![A static star](./images/staticStar.gif)

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

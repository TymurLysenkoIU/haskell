{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

r_0 = 0.6
d = 2.0
n = 20.0
numStars = 50

r :: Double -> Double
r i = r_0 + (i * (d / n))

theta :: Double -> Double
theta i = (2 * i * pi) / n

x :: Double -> Double
x i = (r i) * (cos (theta i))

y :: Double -> Double
y i = (r i) * (sin (theta i))

-- | Generate a list of points for a spiral.
spiral :: Integer -> [(Double, Double)]
spiral 0 = []
spiral i = (x (fromIntegral i), y (fromIntegral i)) : spiral (i - 1)

-- | A spiral picture where every intermediate point
-- is rendered using a given picture.
-- The parameter specifies total number of points in a spiral.
drawSpiral :: Integer -> Picture -> Picture
drawSpiral numPoints pic = go (spiral numPoints)
  where
    go []       = blank
    go ((x, y) : ps) = translated x y (pic) <> go ps

-- | Render a star.
star :: Picture
star = colored purple (solidCircle 0.1)

-- | Render a black hole.
blackHole :: Picture
blackHole = colored black (solidCircle 0.5)

-- | Render a galaxy with a black hole
-- and two spiral arms.
galaxy :: Picture
galaxy = blackHole <> baseSpiral <> rotated pi baseSpiral
  where
    baseSpiral = drawSpiral numStars star

main :: IO ()
main = drawingOf galaxy

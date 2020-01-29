{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
-- | N-body simulation.
module Main where

import CodeWorld

main :: IO ()
main = -- drawingOf (drawSystem solarSystem)
  simulationOf solarSystem updateSystem drawSystem

sun :: Body
sun = Body (0, 0) (0, 0) m (Circle (massToRadius m)) yellow
  where m = 2e30

earth :: Body
earth = Body (fromPointer (d, d)) (0, 0) m (Circle (massToRadius m)) blue
  where
    m = 6e24
    d = 14733e4

solarSystem :: System
solarSystem = System [sun, earth]

-- * Types

-- | Mass (in kilograms).
type Mass = Double

-- | Position (coordinates in meters).
type Position = (Double, Double)

-- | Velocity vector (components in meters per second).
type Velocity = (Double, Double)

-- | Shape of a body
data Shape = Circle Double

-- | An astronomical body (e.g. a planet or an asteroid).
data Body = Body Position Velocity Mass Shape Color -- TODO: color shape

-- | A system of bodies.
data System = System [Body]

-- * Helpers

-- mapBodies :: (Body -> a) -> [Body] -> [a]
-- mapBodies _f []    = []
-- mapBodies f (b:bs) = f b : mapBodies f bs

-- * Rendering

-- | Render a single body, taking visualisation constants into account.
drawBody :: Body -> Picture
drawBody (Body p _ _ (Circle r) c) =
  translated x y (colored c (solidCircle r))
  where
    ps = (fromPointer p)
    x = fst ps
    y = snd ps
drawBody _ = blank

drawBodies :: [Body] -> Picture
drawBodies = pictures . (map drawBody)

-- | Render a system of bodies on a black background.
drawSystem :: System -> Picture
drawSystem (System bs) = drawBodies bs <> background
  where
    background = solidRectangle 100 100

-- * Physics (updates)

-- | Update body's position based on its velocity.
moveBody :: Double -> Body -> Body
moveBody dt (Body p v m s c) = Body p' v m s c
  where
    p' = p `vectorSum` scaledVector dt v

-- | Update body's position and velocity.
updateBody :: Double -> [Body] -> Body -> Body
updateBody dt bodies = moveBody dt . applyGravity dt bodies

-- | Update all bodies in a system.
updateBodies :: Double -> [Body] -> [Body]
updateBodies dt bodies
  = map (updateBody dt bodies) bodies

-- | Update entire system, taking 'timeScale' into account.
updateSystem :: Double -> System -> System
updateSystem dt (System bs) = System (updateBodies (dt * timeScale) bs)

-- ** Gravity helpers

-- | Acceleration in a two body system.
--
-- NOTE: ignores gravitional effect of "small" objects
gravityAcc
  :: Body     -- ^ Body whose gravitational field is used.
  -> Body     -- ^ Body to compute acceleration for.
  -> Vector
gravityAcc (Body p0 _ m0 _ _) (Body p1 _ _ _ _)
  | p0 == p1  = (0, 0)
  | otherwise = scaledVector (acc / dist) accVecDir
  where
    accVecDir = vectorDifference p0 p1 -- TODO: wrong order
    dist = vectorLength accVecDir
    acc = bigG * m0 / dist

-- | Compute and apply acceleration to update body's velocity.
applyGravity :: Double -> [Body] -> Body -> Body
applyGravity dt bodies body@(Body p (vx, vy) m s c) =
  Body p (vx + (fst resAcc), vy + (snd resAcc)) m s c
  where
    resAccVec = (scaledVector dt) . (gravityAcc body)
    accVecs = map resAccVec bodies
    go [] = (0, 0)
    go (av:avs) = vectorSum av (go avs)
    resAcc = go accVecs

-- * Controls

-- | Handle user input (e.g. mouse clicks).
handleSystem :: Event -> System -> System
handleSystem _ = id -- ignore all events

-- * Helpers

-- | Convert pointer position into coordinates in a simulation.
fromPointer :: Point -> Point
fromPointer (x, y) = (x / viewportScale, y / viewportScale)

-- * Constants

-- ** Physical constants

-- | One astronomical unit (in meters).
au :: Double
au = 149597900000

-- | Gravitational constant.
bigG :: Double
bigG = 6.67430e-11

-- ** Visualisation parameters

-- | Viewport scaling factor.
--
-- For inner solar system: 1 unit = 0.2 'au'.
-- For Earth-Moon: 1 unit = 0.0005 'au'.
viewportScale :: Double
viewportScale = 0.2 * au

-- | Time scale for the simulation.
--
-- For inner solar system: 1 second = 1 week
-- For Earth-Moon: 1 second = 1 day
timeScale :: Double
timeScale = 1

-- | Mass to visualisation radius mapping.
-- For nicer visualisation we use logarithmic scale.
massToRadius :: Mass -> Double
massToRadius m = 0.01 + 3e-7 * (logBase 10 (m + 10)) ^ 4

-- | Smallest mass to take gravity into account for.
smallMassThreshold :: Mass
smallMassThreshold = 1e21

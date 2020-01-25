{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import CodeWorld

-- | The standard RGB function accepts real numbers in range [0; 1].
-- | It is more convenient to create colors by passing integers in range [0; 255].
trueRGB :: Int -> Int -> Int -> Color
trueRGB r g b = RGB ((fromIntegral r) / 255) ((fromIntegral g) / 255) ((fromIntegral b) / 255)

redDimmed :: Color
redDimmed = trueRGB 120 20 20

yellowDimmed :: Color
yellowDimmed = trueRGB 150 140 15

greenDimmed :: Color
greenDimmed = trueRGB 15 120 20

-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Color -> Double -> Picture -> Picture
lightCircle c y texture = translated 0 y (texture <> colored c (solidCircle 1))

-- | Red traffic light for car.
carRedLight :: Picture
carRedLight = lightCircle red 2.4 blank

-- | Red dimmed traffic light for car.
carRedDimmedLight :: Picture
carRedDimmedLight = lightCircle redDimmed 2.4 blank

-- | Red traffic light for pedastrian and cyclist.
pedastrianCyclistRedLight :: Picture
pedastrianCyclistRedLight = lightCircle red 1.2 blank

-- | Red dimmed traffic light for pedastrian and cyclist.
pedastrianCyclistRedDimmedLight :: Picture
pedastrianCyclistRedDimmedLight = lightCircle redDimmed 1.2 blank

-- | Yellow traffic light for car.
carYellowLight :: Picture
carYellowLight = lightCircle yellow 0 blank

-- | Yellow dimmed traffic light for car.
carYellowDimmedLight :: Picture
carYellowDimmedLight = lightCircle yellowDimmed 0 blank

-- | Green traffic light for car.
carGreenLight :: Picture
carGreenLight = lightCircle green (-2.4) blank

-- | Green dimmed traffic light for car.
carGreenDimmedLight :: Picture
carGreenDimmedLight = lightCircle greenDimmed (-2.4) blank

pedastrianGreenLight :: Picture
pedastrianGreenLight = lightCircle green (-1.2) (lettering "\x1F6B6")

pedastrianGreenDimmedLight :: Picture
pedastrianGreenDimmedLight = lightCircle greenDimmed (-1.2) (lettering "\x1F6B6")

cyclistGreenLight :: Picture
cyclistGreenLight = lightCircle green (-1.2) (lettering "\x1F6B2")

cyclistGreenDimmedLight :: Picture
cyclistGreenDimmedLight = lightCircle greenDimmed (-1.2) (lettering "\x1F6B2")

-- | Frame for traffic lights with 3 bulbs.
frame3 :: Picture
frame3 = rectangle 2.5 7.5

-- | Frame for traffic lights with 2 bulbs.
frame2 :: Picture
frame2 = rectangle 2.5 5

-- | Simple traffic lights.
carTrafficLight :: Int -> Picture
carTrafficLight second
  | second <= 3 = carGreenLight       <> carYellowDimmedLight <> carRedDimmedLight <> frame3
  | second <= 4 = carGreenDimmedLight <> carYellowLight       <> carRedDimmedLight <> frame3
  | second <= 7 = carGreenDimmedLight <> carYellowDimmedLight <> carRedLight       <> frame3
  | second == 8 = carGreenDimmedLight <> carYellowLight       <> carRedLight       <> frame3
  | otherwise   = carGreenDimmedLight <> carYellowDimmedLight <> carRedDimmedLight <> frame3

-- | Traffic lights controller switching lights.
carTrafficController :: Double -> Picture
carTrafficController t = carTrafficLight ((floor t) `mod` 9)

traficLight2 :: Picture -> Picture -> Picture -> Picture -> Int -> Picture
traficLight2 green greenDimmed red redDimmed second
  | second <= 3 = green       <> redDimmed <> frame2
  | second <= 4 = greenDimmed <> redDimmed <> frame2
  | second <= 8 = greenDimmed <> red       <> frame2
  | otherwise   = greenDimmed <> redDimmed <> frame2

pedastrianTrafficLight :: Int -> Picture
pedastrianTrafficLight second =
  traficLight2 pedastrianGreenLight pedastrianGreenDimmedLight pedastrianCyclistRedLight pedastrianCyclistRedDimmedLight second

cyclistTrafficLight :: Int -> Picture
cyclistTrafficLight second =
  traficLight2 cyclistGreenLight cyclistGreenDimmedLight pedastrianCyclistRedLight pedastrianCyclistRedDimmedLight second

trafficController2Generator :: (Int -> Picture) -> Double -> Picture
trafficController2Generator trafficLight t = trafficLight ((floor t) `mod` 9)

pedastrianController :: Double -> Picture
pedastrianController t = trafficController2Generator pedastrianTrafficLight t

cyclistController :: Double -> Picture
cyclistController t = trafficController2Generator cyclistTrafficLight t

solutionCarTrafficLight :: IO ()
solutionCarTrafficLight = animationOf carTrafficController

solutionPedastrianCyclistTrafficLight :: IO ()
solutionPedastrianCyclistTrafficLight = animationOf (\t -> pedastrianController t <> translated 3 0 (cyclistController t))

main :: IO ()
main = solutionPedastrianCyclistTrafficLight

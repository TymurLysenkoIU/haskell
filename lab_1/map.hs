{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

floorTile :: Picture
floorTile = colored yellow (solidRectangle 1 1)

wallTile :: Picture
wallTile = colored black (solidRectangle 1 1)

redButtonTile :: Picture
redButtonTile =
  colored red (solidCircle 1) <>
  floorTile

redDoorTile :: Picture
redDoorTile =
  colored red (solidCircle 1) <>
  wallTile

blueButtonTile :: Picture
blueButtonTile =
  colored blue (solidCircle 1) <>
  floorTile

blueDoorTile :: Picture
blueDoorTile =
  colored blue (solidCircle 1) <>
  wallTile

exitTile :: Picture
exitTile =
  colored red (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile n
  | n == 0 = floorTile
  | n == 1 = wallTile
  | n == 2 = exitTile
  | n == 3 = blueButtonTile
  | n == 4 = blueDoorTile
  | n == 5 = redButtonTile
  | n == 6 = redDoorTile
  | otherwise = blank

levelMap :: (Integer, Integer) -> Integer
levelMap (x, y) 
  | (x == 0  && y <= 10)  ||
    (x == 10 && y <= 10)  ||
    (y == 0  && x <= 10)  ||
    (y == 10 && x <= 10)  ||
    (x == 0  && y == 0)   ||
    (x == 10 && y == 10)  ||
    (y == 6  && x <= 6)   ||
    (x == 6  && y <= 10)
    = 1
  | otherwise = 0

tileFromMapAt :: ((Integer, Integer) -> Integer) -> (Integer, Integer) -> Picture
tileFromMapAt lvlMap (x, y) =
  translated (fromIntegral x) (fromIntegral y) (drawTile (lvlMap (x, y)))

computeMapOfSizeLoop :: ((Integer, Integer) -> Integer) -> (Integer, Integer) -> (Integer, Integer) -> Picture
computeMapOfSizeLoop lm (xCur, yCur) (xSize, ySize)
  | yCur == ySize = blank
  | xCur == xSize = computeMapOfSizeLoop lm (0, yCur + 1) (xSize, ySize)
  | xCur <  xSize = tileFromMapAt lm (xCur, yCur) <> computeMapOfSizeLoop lm (xCur + 1, yCur) (xSize, ySize)

computeMapOfSize :: ((Integer, Integer) -> Integer) -> (Integer, Integer) -> Picture
computeMapOfSize lvlMap (xSize, ySize) = computeMapOfSizeLoop lvlMap (0, 0) (xSize, ySize)


levelMapPicture :: Picture
levelMapPicture = computeMapOfSize levelMap (11, 11)

main :: IO ()
main = drawingOf levelMapPicture

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import CodeWorld

tree :: Point -> Point -> Point -> Int -> Double -> Picture
tree p1 p2 p3 n leavesMult = go p1 p2 p3 0
  where
    go :: Point -> Point -> Point -> Int -> Picture
    go (x1, y1) (x2, y2) (x3, y3) i
      | i == n    = colored green (solidCircle (fromIntegral i * leavesMult))
      | otherwise =
          solidPolygon [(x1, y1), (x2, y2), (x3, y3)] <>
          translated translateXLeft translateYLeft (leftBranch) <>
          translated translateXRight translateYRight (rightBranch)
      where
        nX1             = x1
        nY1             = y1
        nX2             = x3 -- == x2 / 2
        nY2             = y2
        nX3             = nX2 / 2.0
        nY3             = y3 / 2.0
        baseMidpoint    = (x3, y1)
        oppositeCathet  = dist (x3, y3) baseMidpoint
        adjacentCathet  = dist (x1, y1) baseMidpoint
        hypothenus      = dist (x1, y1) (x3, y3)
        adjacentAngle   = baseAngle oppositeCathet adjacentCathet
        newBase         = dist (nX1, nY1) (nX2, nY2)
        transHypothenus = hypothenus - newBase
        translateXLeft  = getAdjacentCathet transHypothenus adjacentAngle
        translateYLeft  = getOppositeCathet transHypothenus adjacentAngle
        translateXRight = translateXLeft + (x3 - translateXLeft)
        translateYRight = translateYLeft + (y3 - translateYLeft)
        segment         = go (nX1, nY1) (nX2, nY2) (nX3, nY3) (i + 1)
        leftBranch      = rotated adjacentAngle segment
        rightBranch     = rotated (-adjacentAngle) segment

getAdjacentCathet :: Double -> Double -> Double
getAdjacentCathet hypothenus adjacentAngle = hypothenus * (cos adjacentAngle)

getOppositeCathet :: Double -> Double -> Double
getOppositeCathet hypothenus adjacentAngle = hypothenus * (sin adjacentAngle)

baseAngle :: Double -> Double -> Double
baseAngle oppositeCathet adjacentCathet = atan (oppositeCathet / adjacentCathet)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt (((x1 - x2) ** 2.0) + ((y1 - y2) ** 2.0))

main :: IO ()
main = drawingOf (tree (0, 0) (10, 0) (5, 30) 10 0.03)

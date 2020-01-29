{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
import CodeWorld

tree :: Double -> Picture
tree age = go (0, 0) (5, 0) (2.5, 7.5) 0.0
  where
    go :: Point -> Point -> Point -> Double -> Picture
    go p1@(x1, y1) p2@(x2, y2) p3@(x3, y3) i
      | i >= age = colored green (solidCircle (0.5 * leavesScale))
      | scale <= 1.0 = scaled scale scale (
          translated x3 y3 leftBranch <>
          solidPolygon [p1, p2, p3]
        )
      | otherwise =
          solidPolygon [p1, p2, p3] <>
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
        oppositeLeg     = dist (x3, y3) baseMidpoint
        adjacentLeg     = dist (x1, y1) baseMidpoint
        hypotenuse      = dist (x1, y1) (x3, y3)
        adjacentAngle   = baseAngle oppositeLeg adjacentLeg
        newBase         = dist (nX1, nY1) (nX2, nY2)
        transHypotenuse = hypotenuse - newBase
        translateXLeft  = getAdjacentLeg transHypotenuse adjacentAngle
        translateYLeft  = getOppositeLeg transHypotenuse adjacentAngle
        translateXRight = translateXLeft + (x3 - translateXLeft)
        translateYRight = translateYLeft + (y3 - translateYLeft)
        nextSegment     = go (nX1, nY1) (nX2, nY2) (nX3, nY3) (i + 1)
        leftBranch      = rotated adjacentAngle nextSegment
        rightBranch     = rotated (-adjacentAngle) nextSegment
        scale           = age - i
        leavesScale     = (age - fromInteger (floor age))

getAdjacentLeg :: Double -> Double -> Double
getAdjacentLeg hypotenuse adjacentAngle = hypotenuse * (cos adjacentAngle)

getOppositeLeg :: Double -> Double -> Double
getOppositeLeg hypotenuse adjacentAngle = hypotenuse * (sin adjacentAngle)

baseAngle :: Double -> Double -> Double
baseAngle oppositeLeg adjacentLeg = atan (oppositeLeg / adjacentLeg)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt (((x1 - x2) ** 2.0) + ((y1 - y2) ** 2.0))

growingTree :: Double -> Picture
growingTree t = tree (treeDepth * (abs (sin (t / (treeDepth / 2)))))
  where treeDepth = 6

main :: IO ()
main = animationOf growingTree

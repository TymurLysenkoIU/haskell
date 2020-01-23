{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

treeFractal :: Int -> Picture
treeFractal 0 = blank
treeFractal n = stem <> translated 0 1 (left <> right)
  where
    tree' = treeFractal (n - 1)
    left  = rotated 0.2 (tree')
    right = rotated (-0.2) (tree')
    stem  = polyline [(0, 0), (0, 1)]

main :: IO ()
main = drawingOf (treeFractal 5)

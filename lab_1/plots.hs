{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

histogramLoop :: (Double -> Double -> Picture) -> [Double] -> Int -> Picture
histogramLoop hist (x : xs) i = (hist (fromIntegral i) x) <> (histogramLoop hist xs (i + 1))
histogramLoop hist [] i       = blank

-- | Render data using vertical bars.
bars :: [Double] -> Picture
bars ld = histogramLoop (\x val -> translated x (val / 2) (solidRectangle 1 val)) ld 0

-- | Render data using bold dots.
dots :: [Double] -> Picture
dots ld = histogramLoop (\x val -> translated x val (solidCircle 0.3)) ld 0

-- | Render data as a line.
line :: [Double] -> Picture
line ld = thickPolyline 0.1 (go 0 ld)
  where
    go :: Int -> [Double] -> [Point]
    go x [] = []
    go x (y : ys) = (fromIntegral x, y) : go (x + 1) ys

-- | Render data as an area under the line.
area :: [Double] -> Picture
area ld = solidPolygon (go 0 ld)
  where
    go :: Int -> [Double] -> [Point]
    go x [] = [(fromIntegral (x - 1), 0), (0, 0)]
    go x (y : ys) = (fromIntegral x, y) : go (x + 1) ys

manyFigures :: ([Double] -> Picture) -> [(Color, [Double])] -> Picture
manyFigures plotter [] = blank
manyFigures plotter ((c, d) : tail) = (colored c (plotter d)) <> (manyFigures plotter tail)

-- | Render multiple lines with different colors.
manyLines :: [(Color, [Double])] -> Picture
manyLines l = manyFigures line l

-- | Render multiple areas with different colors.
-- Areas are "stacked" on top of each other.
manyAreas :: [(Color, [Double])] -> Picture
manyAreas l = manyFigures area l

dataList :: [Double]
dataList = [3.0, 5.8, 1.2, 0.7, 9.1, 5.0]

-- | Sample data split into 3 groups.
sampleMany :: [(Color, [Double])]
sampleMany = [
    (red, [1.2, 2.0, 2.5, 3.3, 2.1, 1.5, 0.5, 0.0, 0.0, 0.8]),
    (orange, [2.1, 1.9, 0.0, 0.0, 2.1, 4.0, 3.1, 3.9, 3.5, 2.3]),
    (yellow, [2.9, 1.1, 3.3, 0.1, 1.0, 1.5, 1.3, 1.7, 1.9, 2.0])
  ]

picture :: Picture
picture =
  coordinatePlane <>
  -- manyLines sampleMany <>
  manyAreas sampleMany <>
  -- (colored blue (dots dataList)) <>
  -- (colored yellow (area dataList)) <>
  -- (colored red (line dataList)) <>
  -- (colored green (bars dataList)) <>
  blank

main :: IO ()
main = drawingOf picture

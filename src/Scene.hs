module Scene where

import qualified Graphics.Rendering.Cairo as C

type Disc = (Double, Double) -- (x, y)

-- Assume unit radius
genPyramid :: Integer -> [Disc]
genPyramid levels =
    concat [gen level | level <- [0 .. (levels - 1)]]
        where
          gen level = [(xpos (fromInteger level) (fromInteger x),
                        ypos $ fromInteger level) | x <- [0 .. level]]
          ypos level = sqrt((2 * level)^2 - level^2) + 1
          xpos level i = fromInteger levels - level + 2 * i
                   
renderDiscs :: Double -> [Disc] -> C.Render ()
renderDiscs scale discs = do
  C.setSourceRGB 0 0 0
  C.setLineWidth 1

  let scaled = map (\(x, y) -> (x * scale, y * scale)) discs
  mapM_ renderDisc scaled
    where
      renderDisc (x, y) = do
        C.arc x y scale 0 (2 * pi)
        C.stroke

discsToSVG :: Integer -> FilePath -> IO ()
discsToSVG discs filename =
  C.withSVGSurface filename 300 300 renderer
    where
      renderer surface =
        C.renderWith surface $ renderDiscs 10 $ genPyramid discs

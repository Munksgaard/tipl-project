module Render where

import Disc

import qualified Graphics.Rendering.Cairo as C

renderDiscs :: Double -> [Disc] -> C.Render ()
renderDiscs scaler discs = do
  C.setSourceRGB 0 0 0
  C.setLineWidth 1
  --
  let scaled = map (\d -> (xpos d * scaler, ypos d * scaler)) discs
  mapM_ renderDisc scaled
    where
      renderDisc (x, y) = do
        C.arc x y scaler 0 (2 * pi)
        C.stroke

discsToSVG :: [Disc] -> FilePath -> IO ()
discsToSVG discs filename =
  C.withSVGSurface filename 300 300 renderer
    where
      renderer surface =
        C.renderWith surface $ renderDiscs 10 discs

pyramidToSVG :: Integer -> FilePath -> IO ()
pyramidToSVG levels filename = discsToSVG pyramid filename
    where
        pyramid = genPyramid levels

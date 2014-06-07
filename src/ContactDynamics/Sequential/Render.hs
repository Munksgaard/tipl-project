module ContactDynamics.Sequential.Render where

import ContactDynamics.Sequential.Disc
import ContactDynamics.Sequential.Contact
import ContactDynamics.Sequential.JacobiSolver

import Numeric.LinearAlgebra
import qualified Graphics.Rendering.Cairo as C

renderDiscs :: Double -> [Disc] -> C.Render ()
renderDiscs scaler discs = do
  C.setSourceRGB 0 0 0
  C.setLineWidth 1
  --
  let scaled = map (\d -> (xpos d * scaler, ypos d * scaler, discId d)) discs
  let maxy = maximum $ map (\(_, x, _) -> x) scaled
  mapM_ (renderDisc maxy) scaled
    where
      renderDisc maxy (x, y, i) = do
        C.arc x (maxy - y + scaler) scaler 0 (2 * pi)
        C.moveTo (x-3) $ maxy - y + 13
        C.textPath $ show i
        C.stroke

renderContacts :: Double -> [(Contact, Vector Double)] -> C.Render ()
renderContacts scaler xs = do
  C.setSourceRGB 0 0 0
  C.setLineWidth 1
  --
  let scaled = map (\((cd, an), r) ->
                        ((xpos cd * scaler, ypos cd * scaler),
                         (xpos an * scaler, ypos an * scaler),
                         r @>0)) xs
  let maxy = maximum $ map (\(_, x, _) -> x) scaled
  mapM_ renderImpulse scaled
    where
      renderImpulse ((x1, y1), (x2, y2), width) = do
        C.moveTo x1 $ scaler - y1 + scaler
        C.setLineWidth $ width * scaler
        C.lineTo x2 $ scaler - y2 + scaler
        C.stroke

discsToSVG :: [Disc] -> FilePath -> IO ()
discsToSVG discs filename =
  C.withSVGSurface filename 300 300 renderer
    where
      renderer surface =
        C.renderWith surface $ renderDiscs 10 discs

pyramidToSVG :: Integer -> FilePath -> IO ()
pyramidToSVG levels = discsToSVG pyramid
    where
        pyramid = genPyramid levels

contactsToSVG :: Integer -> FilePath -> IO ()
contactsToSVG levels filename = do
  C.withSVGSurface filename (levels * scaler * 2) (levels * scaler * 2) renderer
  print $ show rs
    where
      scaler = 30
      renderer surface = do
        C.renderWith surface $ renderDiscs (fromInteger scaler) discs
        C.renderWith surface $ renderContacts (fromInteger scaler) $ zip cs rs
      cs = contacts discs
      rs = jacobi 100 discs $ fromList [0,-1,0,0,0,0]
      discs = genPyramid levels

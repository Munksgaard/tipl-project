module ContactDynamics.Render where

import ContactDynamics.Disc
import ContactDynamics.Contact
import ContactDynamics.Utils

import ContactDynamics.Sequential.JacobiSolver
import ContactDynamics.Accelerate.BNLJSolver

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
  let maxy = maximum $ map (\((_, y1), (_, y2), _) -> max y1 y2) scaled
  mapM_ (renderImpulse maxy) scaled
    where
      renderImpulse maxy ((x1, y1), (x2, y2), width) = do
        C.moveTo x1 $ maxy - y1 + scaler
        C.setLineWidth $ width * scaler
        C.lineTo x2 $ maxy - y2 + scaler
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

contactsToSVG :: (Int -> [Disc] -> [Vector Double] -> [Vector Double]) ->
                 Integer -> FilePath -> Int -> IO ()
contactsToSVG computer levels filename iters =
  C.withSVGSurface filename (fromInteger $ levels * scaler * 2)
      (fromInteger $ levels * scaler * 2)
      renderer
  where
    scaler = 30
    renderer surface = do
      C.renderWith surface $ renderDiscs (fromInteger scaler) discs
      C.renderWith surface $ renderContacts (fromInteger scaler) $ zip cs rs
    cs = contacts discs
    rs = computer iters discs $ extF $ length cs
    discs = genPyramid levels

gaussSVG :: Integer -> FilePath -> [Disc] -> [Contact] -> [Vector Double] -> IO ()
gaussSVG levels filename ds cs rs =
  C.withSVGSurface filename (fromInteger $ levels * scaler * 2)
      (fromInteger $ levels * scaler * 2)
      renderer
  where
      scaler = 30
      renderer surface = do
        C.renderWith surface $ renderDiscs (fromInteger scaler) ds
        C.renderWith surface $ renderContacts (fromInteger scaler) $ zip cs rs

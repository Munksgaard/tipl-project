module Scene where

import qualified Graphics.Rendering.Cairo as C
import Data.List

type Disc = (Double, Double) -- (x, y)
type Contact = (Int, Int)
type Scene = ([Disc], [Contact])

-- Error margin for distances
epsilon :: Double
epsilon = 0.0001

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
  --
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

-- Standard euclidian 2d distance
dist :: Disc -> Disc -> Double
dist (x1, y1) (x2, y2) =
    sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Returns a list of contacts for the given set of discs.
contacts :: [Disc] -> [Contact]
contacts discs = contacts' discs 0
    where
      contacts' [] _ = []
      contacts' (x:xs) i = map (\d -> (i, 1 + i + d)) (findIndices (contactp x) xs)
                   ++ contacts' xs (1 + i)
      contactp d1 d2 = dist d1 d2 <= 2 + epsilon

genScene :: Integer -> Scene
genScene n =
    let discs = genPyramid n
    in (discs, contacts(discs))

module Scene where

import qualified Graphics.Rendering.Cairo as C
import Data.List

type Disc = (Double, Double) -- (x, y)
type Contact = (Int, Int)
type Scene = ([Disc], [Contact])

type Vector a = [a]
type Matrix a = [Vector a]

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
      contacts' (x:xs) i = map (\d -> (i, 1 + i + d))
                           (findIndices (contactp x) xs)
                           ++ contacts' xs (1 + i)
      contactp d1 d2 = dist d1 d2 <= 2 + epsilon

genScene :: Integer -> Scene
genScene n =
    let discs = genPyramid n
    in (discs, contacts discs)

-- Finds indices of bodies adjacent to bodies in a contact
-- Assumes (i1, i2) == (j1, j2) iff it is the same contact
adjConts :: Contact -> [Contact] -> [Int]
adjConts c1@(an1, cd1) conts =
   nub $ map otherContact $ filter adj conts
    where
      otherContact (an2, cd2) = if an1 == an2 || cd1 == an2 then cd2 else an2
      adj c2@(an2, cd2) = (an1 == an2 || an1 == cd2 || cd1 == an2 || cd1 == cd2)
                          && c1 /= c2

-- (Somewhat pretty) print matrix
printMatrix :: Show a => Matrix a -> IO ()
printMatrix = mapM_ print

-- Concatenate two matrices
concMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
concMatrix m1 m2 =
    let len1 = length m1
        len2 = length m2
        m1' = map (\row -> row ++ replicate len2 0) m1
        m2' = map (\row -> replicate len1 0 ++ row) m2
    in m1' ++ m2'

-- More efficient for a list of matrices
concMatrices :: Num a => [Matrix a] -> Matrix a
concMatrices ms =
    let (m, _) = foldr conc ([], 0) $ zip ms lens in m
    where
      lens = map (length . head) ms
      totalLen = sum lens
      conc (x, len) (xs, i) =
          let x' = map (\row -> replicate (totalLen - i - len) 0
                        ++ row
                        ++ replicate i 0) x
          in (x' ++ xs, i + len)

-- Perform matrix multiplication
matmult :: Num a => Matrix a -> Matrix a -> Matrix a
matmult m1 m2 =
    map rowmult m1
    where
      m2' = transpose m2
      rowmult row =
          map (\col -> sum $ zipWith (*) row col) m2'

module Scene where

import Data.List
import Numeric.LinearAlgebra

type Disc = (Double, Double) -- (x, y)
type Contact = (Int, Int)
type Scene = ([Disc], [Contact])
type Radius = Double

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
          xpos level n = fromInteger levels - level + 2 * n

-- Standard euclidian 2d distance
dist :: Disc -> Disc -> Double
dist (x1, y1) (x2, y2) =
    sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Returns a list of contacts for the given set of discs.
contacts :: [Disc] -> [Contact]
contacts discs = contacts' discs 0
    where
      contacts' [] _ = []
      contacts' (x:xs) n = map (\d -> (n, 1 + n + d))
                           (findIndices (contactp x) xs)
                           ++ contacts' xs (1 + n)
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

-- -- Concatenate two matrices
concMatrix :: (Num a, Element a) => Matrix a -> Matrix a -> Matrix a
concMatrix m1 m2 =
    let (col1, row1) = (cols m1, rows m1)
        (col2, row2) = (cols m2, rows m2)
        ur = (row1><col2) (repeat 0)
        dl = (row2><col1) (repeat 0)
    in
      fromBlocks [[m1, ur], [dl, m2]]

-- Should perhaps be rewritten for performance?
concMatrices :: (Num a, Element a) => [Matrix a] -> Matrix a
concMatrices = foldr1 concMatrix

contactMatrix :: Contact -> [Disc] -> Radius -> Matrix Double
contactMatrix (an', cd') ds r =
    let (an, cd) = (ds !! an', ds !! cd')
        x = fst cd - fst an
        y = snd cd - snd an
        h = sqrt $ x^2 + y^2
        angle = acos $ x / h
        c = cos angle
        s = sin angle
    in (6><2) [ c, -s,
                s,  c,
                0, -r,
               -c,  s,
               -s, -c,
                0, -r]

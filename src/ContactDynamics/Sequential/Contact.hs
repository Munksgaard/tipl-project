module ContactDynamics.Sequential.Contact where

import ContactDynamics.Sequential.Disc

import Data.List
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

type Contact = (Disc, Disc)

-- Error margin for distances
epsilon :: Double
epsilon = 0.0001

contacts :: [Disc] -> [Contact]
contacts [] = []
contacts (x:xs) = map (\d -> (x, d))
                  (filter (contactp x) xs)
                  ++ contacts xs
    where
      contactp d1 d2 = dist d1 d2 <= (radius d1 + radius d2) + epsilon

-- Finds indices of bodies adjacent to bodies in a contact
-- Assumes (i1, i2) == (j1, j2) iff it is the same contact
discContacts :: Disc -> [Contact] -> [Int]
discContacts d =
   findIndices (\(d', d'') -> d == d' || d == d'')

adjContacts :: Contact -> [Contact] -> Int -> [Int]
adjContacts c@(d1, d2) cs n =
    sort $ delete n $ nub $ discContacts d1 cs ++ discContacts d2 cs

contactMatrix :: Contact -> Matrix Double
contactMatrix (cd, an) =
    let phi = angle an cd
        c = cos phi
        s = sin phi
    in trans $ (2><6) [ c, s, 0, -c, -s, 0
                      , -s, c, -(radius cd), s, -c, -(radius an)]

pick :: Show a => [a] -> [Int] -> [a]
pick xs ns =
    pick' xs ns 0
        where
          pick' _ [] _ = []
          pick' (x:xs') (n:ns') i | n == i = x : pick' xs' ns' (i + 1)
                                  | otherwise = pick' xs' (n:ns') (i+1)
          pick' xs' ns' i = error $ show (show xs', show ns', show i)

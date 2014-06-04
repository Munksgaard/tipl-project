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
discContacts :: Disc -> [Contact] -> [Contact]
discContacts d =
    filter (\(d', d'') -> d == d' || d == d'')

adjContacts :: Contact -> [Contact] -> [Contact]
adjContacts c@(d1, d2) cs =
    delete c $ nub $ discContacts d1 cs ++ discContacts d2 cs

contactMatrix :: Contact -> Matrix Double
contactMatrix (cd, an) =
    let phi = angle cd an
        c = cos phi
        s = sin phi
    in (6><2) [ c, -s,
                s,  c,
                0,  -(radius cd),
               -c,  s,
               -s, -c,
                0,  radius an]

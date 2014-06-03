module Contacts where

import Disc

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
                0,  -(radius cd), -- TODO: Find out what r is
               -c,  s,
               -s, -c,
                0,  radius an]

waa :: Contact -> Matrix Double -- 2x2 matrix
waa (cd, an) =
    trans h `multiply` m `multiply` h
    where
        h = contactMatrix (cd, an)
        m = diagBlock [massM cd, massM an]

wab :: Contact -> Contact -> Matrix Double -- 2x2 matrix
wab (cd1, an1) (cd2, an2) =
    trans h_alpha `multiply` m `multiply` h_beta
    where
        h_alpha = undefined -- 3x2 matrix
        m = undefined -- 3x3 matrix
        h_beta = undefined -- 3x2

-- delassus :: Contact -> [Contact] -> Matrix Double
-- delassus (cd, an) cs =
--     trans h `multiply` inv m `multiply` h
--     where
--       h = diagBlock $ map (aux contactMatrix (!)) cs
--       m = diagBlock $ map (aux massM (!)) cs
--       aux f1 f2 (t1, t2) = f2 (f1 (t1, t2)) (f1 (t2, t1))

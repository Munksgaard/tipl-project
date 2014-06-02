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
    in (6><3) [ c, -s, 0,
                s,  c, 0,
                0,  0, 0, -- TODO: Find out what r is
               -c,  s, 0,
               -s, -c, 0,
                0,  0, 0]

relVelocity :: Contact -> Matrix Double
relVelocity (cd, an) =
    h `multiply` v
    where
      v = linearV cd # angularV cd # linearV an # angularV an
      h = trans $ contactMatrix (cd, an) # contactMatrix (an, cd)

-- delassus :: Contact -> [Contact] -> Matrix Double
-- delassus (cd, an) cs =
--     trans h `multiply` inv m `multiply` h
--     where
--       h = diagBlock $ map (aux contactMatrix (!)) cs
--       m = diagBlock $ map (aux massM (!)) cs
--       aux f1 f2 (t1, t2) = f2 (f1 (t1, t2)) (f1 (t2, t1))

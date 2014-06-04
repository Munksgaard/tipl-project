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
      -- Normalize contacts: put the relevant disc as the candidate
      alpha@(cd, _) = if cd1 == cd2 || cd1 == an2
                      then (cd1, an1)
                      else (an1, cd1)
      beta = if cd == cd2 then (cd2, an2) else (an2, cd2)
      h_alpha = takeRows 3 $ contactMatrix alpha -- 3x2 matrix
      m = diagBlock [massM cd] -- 3x3 matrix
      h_beta = takeRows 3 $ contactMatrix beta -- 3x2

sumWab :: Contact -> [Contact] -> Matrix Double
sumWab c cs =
    let adj = map (wab c) $ adjContacts c cs
    in foldr add (zeros 2 2) adj

module ContactDynamics.Sequential.Contact where

import ContactDynamics.Disc
import ContactDynamics.Utils
import ContactDynamics.Contact

import Data.List
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

-- Finds indices of bodies adjacent to bodies in a contact
-- Assumes (i1, i2) == (j1, j2) iff it is the same contact
discContacts :: Disc -> [Contact] -> [Int]
discContacts d =
   findIndices (isIn d)

adjContacts :: Contact -> [Contact] -> Int -> [Int]
adjContacts c@(d1, d2) cs n =
    sort $ delete n $ nub $ discContacts d1 cs ++ discContacts d2 cs

module ContactDynamics.Sequential.JacobiSolver where

import ContactDynamics.Sequential.Disc
import ContactDynamics.Sequential.Contact

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

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

sumWab :: Contact -> [Contact] -> Vector Double -> Vector Double
sumWab c cs r =
    foldr add (fromList [0, 0]) prods
        where
          prods = zipWith mXv wabs (repeat r)
          wabs = map (wab c) cs

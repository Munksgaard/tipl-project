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

jacobi :: [Disc] -> Vector Double -> [Vector Double]
jacobi ds ext =
    iter 10 r_init
        where
          cs = contacts ds
          adjs = map (`adjContacts` cs) cs
          r_init = replicate (length cs) $ fromList [0, 0]
          --
          iter :: Int -> [Vector Double] -> [Vector Double]
          iter 0 rs = rs
          iter k rs = iter (k-1) r_new
              where
                (cd1, an1) = head cs
                (cd2, an2) = head $ tail cs
                firstM = diagBlock [massM cd1, massM an1] -- 6x6
                secondM = diagBlock [massM cd2, massM an2] -- 6x6
                first = trans (contactMatrix (cd1, an1)) `multiply` firstM `mXv` ext -- 2x1
                second = trans (contactMatrix (cd2, an2)) `multiply` secondM `mXv` ext -- 2x1
                rhss = first : second : (drop 2 $ zipWith3 sumWab cs adjs rs)
                waas = map waa cs
                solver rhs waa =
                    fromList
                      (if (-(rhs @> 0)) < 0 then
                           [inv waa `mXv` rhs @> 0, waa @@> (1, 1)]
                       else [0, waa @@> (1, 1)])
                r_new = zipWith solver rhss waas

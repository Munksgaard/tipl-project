module ContactDynamics.Contact where

import ContactDynamics.Disc
import ContactDynamics.Utils

import Numeric.LinearAlgebra

type Contact = (Disc, Disc)

contactMatrix :: Contact -> Matrix Double
contactMatrix (cd, an) =
  let phi = angle an cd
      c   = cos phi
      s   = sin phi
      r1  = radius cd
      r2  = radius an
  in trans $ (2><6) [  c, s,   0, -c, -s,   0
                    , -s, c, -r1,  s, -c, -r2 ]

contacts :: [Disc] -> [Contact]
contacts [] = []
contacts (x:xs) = map (\d -> (x, d))
                  (filter (contactp x) xs)
                  ++ contacts xs
    where
      contactp d1 d2 = dist d1 d2 <= (radius d1 + radius d2) + epsilon

isIn :: Disc -> Contact -> Bool
isIn d (cd, an) = d == cd || d == an

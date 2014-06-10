module ContactDynamics.Accelerate.Contact where

import ContactDynamics.Disc
import Data.Array.Accelerate as A
import Data.List as List
import Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra.Util

type Contact = (Disc, Disc)

epsilon :: Double
epsilon = 0.0001

---- Calculate the contact space matrix inverses and the adjacent contacts
-- [Disc] = 
liftData :: [Disc] -> (Exp Int, Acc (Array DIM2 Double), Acc(Array DIM4 Double))
liftData ds = (lift n, use inWaas', use wabss')
  where
    cs      = contacts ds
    n       = length cs
    inWaas' = inWaas cs
    wabss'  = wabss cs

contacts :: [Disc] -> [Contact]
contacts [] = []
contacts (x:xs) = List.map (\d -> (x, d))
                   (List.filter (contactp x) xs)
                   ++ contacts xs
  where
    contactp d1 d2 = dist d1 d2 <= (radius d1 + radius d2) + epsilon

inWaas :: [Contact] -> Array DIM2 Double
inWaas cs = A.fromList inWaasSh rawInWaas
  where
    rawInWaas = List.concat $ List.map (M.toList . M.takeDiag . inWaa) cs
    inWaasSh  = Z :.n :.2
    n         = length cs

inWaa :: Contact -> Matrix Double
inWaa (cd, an) =
  inv $ trans h `multiply` inv m `multiply` h
  where
    h = contactMatrix(cd, an)
    m = diagBlock [massM cd, massM an]

wabss :: [Contact] -> Array DIM4 Double
wabss cs = A.fromList wabssSh wabss'
  where
    wabss'  = List.concat $ List.map (wabs cs) cs
    n       = length cs
    wabssSh = Z :.n :. n :. 2 :. 2

adjTo :: Contact -> Contact -> Bool
adjTo alpha@(cd, an) beta
  | alpha == beta     = False -- A contact is not adjacent to itself
  | cd `isIn` beta
    || an `isIn` beta = True
  | otherwise         = False

isIn :: Disc -> Contact -> Bool
isIn d (cd, an) = d == cd || d == an

wabs :: [Contact] -> Contact -> [Double]
wabs cs alpha = flattenMatrices wabs'
  where
    wabs'     = maybeWabsToWab maybeWabs
    maybeWabs = List.map (maybeWab alpha) cs

-- Flatten a matrix into row major order
flattenMatrix :: Matrix Double -> [Double]
flattenMatrix = M.toList . M.flatten

-- Flatten a list of matrices into row major order and append them in row major order
flattenMatrices :: [Matrix Double] -> [Double]
flattenMatrices = List.concat . (List.map flattenMatrix)

maybeWabsToWab :: [Maybe (Matrix Double)] -> [Matrix Double]
maybeWabsToWab = List.map maybeWabToWab

maybeWabToWab :: Maybe (Matrix Double) -> Matrix Double
maybeWabToWab Nothing     = buildMatrix 2 2 (\_ -> 0.0)
maybeWabToWab (Just wab') = wab'

maybeWab :: Contact -> Contact -> Maybe (Matrix Double)
maybeWab alpha beta
  | alpha `adjTo` beta = Just $ wab alpha beta
  | otherwise          = Nothing

wab :: Contact -> Contact -> Matrix Double
wab alpha@(cd1, an1) beta@(cd2, an2) =
  trans h_alpha `multiply` inv m `multiply` h_beta
  where
    common  = if cd1 == cd2 || cd1 == an2 then cd1 else an1
    h_alpha = (if common == cd1 then M.takeRows else M.dropRows) 3 $
              contactMatrix alpha
    h_beta  = (if common == cd2 then M.takeRows else M.dropRows) 3 $
              contactMatrix beta
    m       = massM common

contactMatrix :: Contact -> Matrix Double
contactMatrix (cd, an) =
  let phi = angle an cd
      c   = cos phi
      s   = sin phi
      r1  = radius cd
      r2  = radius an
  in trans $ (2><6) [  c, s,   0, -c, -s,   0
                    , -s, c, -r1,  s, -c, -r2 ]
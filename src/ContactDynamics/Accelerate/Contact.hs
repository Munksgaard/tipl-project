module ContactDynamics.Accelerate.Contact where

import ContactDynamics.Disc
import ContactDynamics.Contact
import Data.Array.Accelerate as A
import Data.List as List
import Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra.Util

---- Calculate the contact space matrix inverses and the adjacent contacts
-- [Disc] =
liftData :: [Disc] -> [M.Vector Double] -> (Int, Exp Int, Acc (Array DIM1 Double), Acc (Array DIM2 Double), Acc(Array DIM4 Double), Acc(Array DIM2 Double))
liftData ds extF = (n, lift n, use w, use inWaas', use wabss', use extF')
  where
    cs          = contacts ds
    n           = length cs
    inWaas'     = inWaas cs
    (w, wabss') = wabss cs
    extF'       = liftExtF n extF cs

-- liftExtF :: Int -> [M.Vector Double] -> [Contact] -> Array DIM2 Double
liftExtF n extF cs = -- H^T M^-1 F
  A.fromList shp $ List.concatMap M.toList $ List.zipWith doExt extF cs
  where
    shp = Z :. n :. 2
    doExt ext c@(cd,an) = scale (-1) $ trans h `multiply` inv m `mXv` ext
      where
        h = contactMatrix c
        m = diagBlock [massM cd, massM an]

inWaas :: [Contact] -> Array DIM2 Double
inWaas cs = A.fromList inWaasSh rawInWaas
  where
    rawInWaas = concatMap (M.toList . M.takeDiag . inWaa) cs
    inWaasSh  = Z :.n :.2
    n         = length cs

inWaa :: Contact -> Matrix Double
inWaa (cd, an) | det waa == 0 = (2><2) [0,0,0,0]
               | otherwise = inv waa
  where
    waa = trans h `multiply` invM `multiply` h
    h = contactMatrix(cd, an)
    invM = inv $ diagBlock [massM cd, massM an]

wabss :: [Contact] -> (Array DIM1 Double, Array DIM4 Double)
wabss cs = (ws'', wabss'')
  where
    ws''          = A.fromList wSh ws'
    wabss''       = A.fromList wabssSh $ List.concat wabss'
    (ws', wabss') = List.unzip $ List.map (wabs cs) cs
    n             = length cs
    wabssSh       = Z :.n :. n :. 2 :. 2
    wSh           = Z :.n

adjTo :: Contact -> Contact -> Bool
adjTo alpha@(cd, an) beta
  | alpha == beta     = False -- A contact is not adjacent to itself
  | cd `isIn` beta
    || an `isIn` beta = True
  | otherwise         = False

wabs :: [Contact] -> Contact -> (Double, [Double])
wabs cs alpha = (w', flattenMatrices wabs')
  where
    (w', wabs') = maybeWabsToWab maybeWabs
    maybeWabs   = List.map (maybeWab alpha) cs

-- Flatten a matrix into row major order
flattenMatrix :: Matrix Double -> [Double]
flattenMatrix = M.toList . M.flatten

-- Flatten a list of matrices into row major order and append them in row major order
flattenMatrices :: [Matrix Double] -> [Double]
flattenMatrices = List.concatMap flattenMatrix

maybeWabsToWab :: [Maybe (Matrix Double)] -> (Double, [Matrix Double])
maybeWabsToWab mwabs = (1.0 / w'', wabs')
  where
    w''         = Prelude.fromIntegral $ foldl' (+) 0 w'
    (w', wabs') = List.unzip $ List.map maybeWabToWab mwabs

maybeWabToWab :: Maybe (Matrix Double) -> (Int, Matrix Double)
maybeWabToWab Nothing     = (0, buildMatrix 2 2 (const 0.0))
maybeWabToWab (Just wab') = (1, wab')

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

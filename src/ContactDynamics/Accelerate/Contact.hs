module ContactDynamics.Accelerate.Contact where

import ContactDynamics.Disc
import Data.Array.Accelerate as A
import Data.List as List

epsilon :: Exp Double
epsilon = constant (0.0001 :: Double)

data AccDiscs = AccDiscs { masses :: Acc (Array DIM2 Double)
                         , pos    :: Acc (Array DIM2 Double)
                         , radii  :: Acc (Vector Double)
                         }

---- Calculate the contact space matrix inverses and the adjacent contacts
-- [Disc] = 
liftData :: [Disc] -> (Exp Int, Acc (Array DIM2 Double), Acc(Array DIM4 Double))
liftData = undefined

liftDiscs :: [Disc] -> AccDiscs
liftDiscs ds = AccDiscs { masses = use $ fromList mSh $ masses'
                        , pos    = use $ fromList pSh $ pos'
                        , radii  = use $ fromList rSh $ radii'
                        }
  where
    masses' = foldl' (++) [] $ List.map (\x -> [mass x, mass x, inertia x]) ds
    pos'    = foldl' (++) [] $ List.map (\x -> [xpos x, ypos x]) ds
    radii'  = foldl' (++) [] $ List.map (\x -> [radius x]) ds
    mSh     = Z :. dn :. 3
    pSh     = Z :. dn :. 2
    rSh     = Z :. dn
    dn      = length ds

radiusMatrix :: Exp Int -> Acc (Vector Double) -> Acc (Array DIM2 Double)
radiusMatrix n r = A.zipWith (\x y -> (x+y)*(x+y)) r' $ A.transpose r'
  where r'    = A.replicate repSh r
        repSh = lift $ Z :.n :.All

distanceMatrix :: Exp Int -> Acc (Array DIM2 Double) -> Acc (Array DIM2 Double)
distanceMatrix n p = A.fold dot d0 diff'
  where
    d0     = constant 0.0 :: Exp Double
    dot    = \x y -> x*x + y*y
    diff'  = A.zipWith (-) pos1' pos2'
    pos1'  = A.replicate repSh1 p
    pos2'  = A.replicate repSh2 $ A.transpose p
    repSh1 = lift $ Z :.n :.All :.All
    repSh2 = lift $ Z :.n :.All :.All

collisionMatrix :: Exp Int -> AccDiscs -> Acc (Array DIM2 Int)
collisionMatrix n ads = A.zipWith (*) idxMask $ A.zipWith collisionCheck r d
  where
    idxMask        = A.zipWith (*) killMask $ A.replicate repSh idxVector
    killMask       = generate killSh killExp
    killSh         = lift $ Z :.n :.n
    killExp        =
      \ix ->
      let
        (Z :.i :.j) = unlift ix
      in
       boolToInt $ i <=* j
    repSh          = lift $ Z :.n :.All
    idxVector      = A.enumFromN idxSh i1
    i1             = constant 1 :: Exp Int
    idxSh          = lift $ Z :.n
    r              = radiusMatrix n $ radii ads
    d              = distanceMatrix n $ pos ads
    collisionCheck = (\x y -> boolToInt $ abs (x - y) <* epsilon)
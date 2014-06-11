module ContactDynamics.Utils where

import Data.List as L
import Numeric.LinearAlgebra as M

-- Computes the external forces for n contacts
extF :: Int -> [M.Vector Double]
extF n = L.replicate 2 (M.fromList [0,-1,0,0,0,0])
                ++ L.replicate (n-2)
                   (M.fromList [0,0,0,0,0,0])

set :: Int -> a -> [a] -> [a]
set 0 x (_:xs) = x:xs
set i x xs =
  take (i-1) xs ++ x : drop i xs

-- Error margin for distances
epsilon :: Double
epsilon = 0.0001

-- Picks out the elements of xs identified by the indices in ns
-- Assumes that ns is sorted
pick :: Show a => [a] -> [Int] -> [a]
pick xs ns =
    pick' xs ns 0
        where
          pick' _ [] _ = []
          pick' (x:xs') (n:ns') i | n == i = x : pick' xs' ns' (i + 1)
                                  | otherwise = pick' xs' (n:ns') (i+1)
          pick' xs' ns' i = error $ show (show xs', show ns', show i)

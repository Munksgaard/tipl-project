module ContactDynamics.Sequential.JacobiSolver where
import ContactDynamics.Sequential.Disc
import ContactDynamics.Sequential.Contact

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

waa :: Contact -> Matrix Double -- 2x2 matrix
waa (cd, an) =
    trans h `multiply` inv m `multiply` h
    where
        h = contactMatrix (cd, an)
        m = diagBlock [massM cd, massM an]

neg :: Container c e => c e -> c e
neg = scale (-1)

wab :: Contact -> Contact -> Matrix Double -- 2x2 matrix
wab alpha@(cd1, an1) beta@(cd2, an2) =
    trans h_alpha `multiply` inv m `multiply` h_beta
    where
      common = if cd1 == cd2 || cd1 == an2 then cd1 else an1
      h_alpha = (if common == cd1 then takeRows else dropRows) 3 $ contactMatrix alpha -- 3x2 matrix
      h_beta = (if common == cd2 then takeRows else dropRows) 3 $ contactMatrix beta -- 3x2 matrix
      m = massM common -- 3x3 matrix

sumWab :: [Contact] -> [Vector Double] -> Contact -> [Int] -> Vector Double
sumWab cs rs c adjs =
    neg $ foldr add (fromList [0, 0]) prods
        where
          prods = zipWith mXv wabs $ pick rs adjs
          wabs = map (wab c) $ pick cs adjs

jacobi :: Int -> [Disc] -> Vector Double -> [Vector Double]
jacobi n ds ext =
    iter n r_init cs ext adjs waas
    where
      cs = contacts ds
      adjs = zipWith (`adjContacts` cs) cs (iterate (+ 1) 0)
      -- r_init = replicate (length cs) $ fromList [0, 0]
      r_init = replicate (length cs) $ fromList [0, 0]
      waas = map waa cs

gauss :: [Disc] -> Vector Double -> [Vector Double] -> [Vector Double]
gauss ds ext rs =
    iter' (length cs - 1) rs cs ext adjs waas
    where
      cs = contacts ds
      adjs = zipWith (`adjContacts` cs) cs (iterate (+ 1) 0)
      -- r_init = replicate (length cs) $ fromList [0, 0]
      waas = map waa cs

iter' (-1) rs _ _ _ _ = rs
iter' i rs cs ext adjs waas  =
    iter' (i-1) (set i new_r rs) cs ext adjs waas
      where
        new_r = solver rhs waa
        waa = waas !! i
        rhs' = sumWab cs rs (cs !! i) (adjs !! i)
        rhs = if i < 2 then
                topStuff ext (cs !! i) + rhs'
              else
                rhs'

set :: Int -> a -> [a] -> [a]
set i x xs =
  take (i-1) xs ++ x : drop i xs


--iter :: Int -> [Vector Double] -> [Vector Double]
iter :: (Eq a, Num a) => a -> [Vector Double] -> [Contact] -> Vector Double -> [[Int]] -> [Matrix Double] -> [Vector Double]
iter 0 rs _ _ _ _ = rs
iter k rs cs ext adjs waas = iter (k-1) r_new cs ext adjs waas
    where
      rhss' = zipWith (sumWab cs rs) cs adjs
      rhss = topStuff ext (head cs) + head rhss'
             : topStuff ext (cs !! 1) + rhss' !! 1
             : drop 2 rhss'
      r_new' = zipWith solver rhss waas
      r_new = zipWith3 (\new old relax -> scale relax new + scale (1-relax) old) r_new' rs
              $ map ((1/) . fromIntegral . length) adjs

solver :: (Ord a, Field a) => Vector a -> Matrix a -> Vector a
solver rhs waa =
    if (rhs @> 0) > 0 then
        inv waa `mXv` rhs
        -- fromList [(inv waa `mXv` rhs @> 0), rhs @> 1 / waa @@> (1,1)]
    else
        fromList [0, 0]

topStuff :: Vector Double -> Contact -> Vector Double
topStuff f (cd, an) =
    neg $ trans h `multiply` inv m `mXv` f
    where
      h = contactMatrix (cd, an)
      m = diagBlock [massM cd, massM an]

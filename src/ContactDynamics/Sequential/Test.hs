module ContactDynamics.Sequential.Test where

import ContactDynamics.Disc
import ContactDynamics.Contact
import ContactDynamics.Utils
import ContactDynamics.Render

import ContactDynamics.Sequential.Contact
import ContactDynamics.Sequential.JacobiSolver

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

ds = genPyramid 3
cs = contacts ds
c1 = head cs
c2 = head $ tail cs
ext = fromList [0,-1,0,0,0,0] : fromList [0,-1,0,0,0,0] : replicate (length cs - 2) (fromList [0,0,0,0,0,0]) :: [Vector Double]

adjs = zipWith (`adjContacts` cs) cs $ iterate (1+) 0

r_init = replicate (length cs) $ fromList [0, 0] :: [Vector Double]
waas = map waa cs


-- iter :: Int -> [Vector Double] -> [Vector Double]
-- iter 0 rs = rs
-- iter k rs = iter (k-1) r_new
--     where
--       rhss' = zipWith3 sumWab cs adjs rs
--       rhss = (topStuff ext $ cs !! 0) `add` (rhss' !! 0)
--              : (topStuff ext $ cs !! 1) `add` (rhss' !! 1)
--              : drop 2 rhss'
--       r_new = zipWith solver rhss waas

-- solver :: Vector Double -> Matrix Double -> Vector Double
-- solver rhs waa =
--     fromList [if ((rhs @> 0)) > 0 then inv waa `mXv` rhs @> 0 else 0,
--               rhs @> 1 / waa @@> (1,1)]

ds2 :: [Disc]
ds2 = [stdDisc { ypos = 2 }, stdDisc { discId = 1}]

cs2 :: [Contact]
cs2 = contacts ds2

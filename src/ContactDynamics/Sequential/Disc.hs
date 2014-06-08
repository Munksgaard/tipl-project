module ContactDynamics.Sequential.Disc where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

data Disc = Disc { mass :: Double
                 , inertia :: Double
                 , xpos :: Double
                 , ypos :: Double
                 , radius :: Double
                 , discId :: Int
                 } deriving (Eq)

instance Show Disc where
    show disc = "<Disc " ++ show (discId disc) ++ " >"

stdDisc :: Disc
stdDisc = Disc { mass = 1
               , inertia = 1
               , xpos = 0
               , ypos = 0
               , radius = 1
               , discId = 0
               }

dist :: Disc -> Disc -> Double
dist d1 d2 =
    sqrt ((xpos d2 - xpos d1)^2 + (ypos d2 - ypos d1)^2)

genPyramid :: Integer -> [Disc]
genPyramid levels =
    zipWith (\i d -> d { discId = i }) (iterate (1+) 0) $
            concat [gen level | level <- [0 .. (levels - 1)]]
        where
          gen level = [stdDisc { xpos = x (fromInteger level) (fromInteger x'),
                                 ypos = y $ fromInteger level } | x' <- [0 .. level]]
          y level = fromInteger levels - sqrt((2 * level)^2 - level^2) + 1
          x level n = fromInteger levels - level + 2 * n

angle :: Disc -> Disc -> Double
angle d1 d2 =
    if y < 0 then
        2 * pi - angle
    else
        angle
    where
      angle = acos $ x / h
      x = xpos d2 - xpos d1
      y = ypos d2 - ypos d1
      h = sqrt $ x^2 + y^2

pos :: Disc -> (Double, Double)
pos d = (xpos d, ypos d)

massM :: Disc -> Matrix Double -- 3x3 diagonal matrix
massM d =
    diag (fromList [mass d, mass d, inertia d])

module Disc where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util

data Disc = Disc { mass :: Double
                 , inertia :: Double
                 , xpos :: Double
                 , ypos :: Double
                 , radius :: Double
                 } deriving (Eq)

instance Show Disc where
    show disc = "<Disc (" ++ truncateShow 2 (xpos disc) ++ ", "
                ++ truncateShow 2 (ypos disc) ++ ")>"
        where
          truncateShow places double = (\(x, y) -> (x ++) $ take places y)
                                       $ break ((== ".") . (:[])) $ show double

stdDisc = Disc { mass = 1
               , inertia = 1
               , xpos = 0
               , ypos = 0
               , radius = 1
               }

dist :: Disc -> Disc -> Double
dist d1 d2 =
    sqrt ((xpos d2 - xpos d1)^2 + (ypos d2 - ypos d1)^2)

genPyramid :: Integer -> [Disc]
genPyramid levels =
    concat [gen level | level <- [0 .. (levels - 1)]]
        where
          gen level = [stdDisc { xpos = x (fromInteger level) (fromInteger x'),
                                 ypos = y $ fromInteger level } | x' <- [0 .. level]]
          y level = sqrt((2 * level)^2 - level^2) + 1
          x level n = fromInteger levels - level + 2 * n

angle :: Disc -> Disc -> Double
angle d1 d2 =
    acos $ x / h
    where
      x = xpos d2 - xpos d1
      y = ypos d2 - ypos d1
      h = sqrt $ x^2 + y^2

massM :: Disc -> Matrix Double
massM d =
    diag (fromList [mass d, mass d, inertia d])

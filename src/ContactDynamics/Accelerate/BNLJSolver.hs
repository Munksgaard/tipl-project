module BNLJSolver where

import Data.Array.Accelerate
import Data.Array.Accelerate.Interpreter as Backend

type VectorList e = Array DIM2 e
type Waa = Array DIM2 Double
type Wabs = Array DIM3 Double

-- Int               = Number of iterations
-- [Disc]            = The discs in the scene
-- Vector Double     = The external force
-- VectorList Double = The resultant impulses on [Disc]
bnljSolver :: Int -> [Disc] -> Vector Double -> VectorList Double
bnljSolver maxIter ds extF = undefined

-- Acc (Vector Double)     = The external force
-- Acc (Wabs)              = The Wab delassus operators for this specific RHS
-- Acc (VectorList Double) = The previously calculated impulses for each contact b corrosponding to the Wabs
-- Acc (Vector Double)     = The right hand side
calcRHS :: Acc (Vector Double) -> Acc (Wabs) -> Acc (VectorList Double) -> Acc (Vector Double)
calcRHS extF wabs impulses = undefined

-- Acc (Waa)           = The inverse of the Waa for this specific RHS
-- Acc (Vector Double) = The right hand side
-- Acc (Vector Double) = The resultant impulse for the contact for this specific RHS
solveRHS Acc (Waa) -> Acc (Vector Double) -> Acc (Vector Double)
solveRHS inWaa rhs = undefined
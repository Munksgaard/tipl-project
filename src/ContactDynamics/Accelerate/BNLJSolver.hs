module BNLJSolver where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as Backend

type VectorList e = Array DIM2 e
type Wxx  = Array DIM2 Double
type Wxxs = Array DIM3 Double

-- Int               = Number of iterations
-- [Disc]            = The discs in the scene
-- Vector Double     = The external force, a 2x1 vector
-- VectorList Double = The resultant impulses on [Disc], a list of 2x1 vectors
-- UNCOMMENTED because of types
--bnljSolver :: Int -> [Disc] -> Vector Double -> VectorList Double
--bnljSolver maxIter ds extF = undefined

---- Calculate right hand side
-- Acc (Vector Double)     = The external force, a 2x1 vector
-- Acc (Wxxs)              = The Wab delassus operators for this specific RHS, a Nx2x2 matrix where N is the number of operators
-- Acc (VectorList Double) = The previously calculated impulses for each contact b corrosponding to the Wabs, a list of 2x1 vectors
-- Acc (Vector Double)     = The right hand side, a 2x1 vector
calcRHS :: Acc (Vector Double) -> Acc (Wxxs) -> Acc (VectorList Double) -> Acc (Vector Double)
calcRHS extF wabs impulses = extF `vadd` (wabs `wXr` impulses)

---- Solve right hand side
-- Acc (Wxx)           = The inverse of the Waa for this specific RHS, a 2x2 vector
-- Acc (Vector Double) = The right hand side, a 2x1 vector
-- Acc (Vector Double) = The resultant impulse for the contact for this specific RHS, a 2x1
solveRHS :: Acc (Wxx) -> Acc (Vector Double) -> Acc (Vector Double)
solveRHS inWaa rhs = undefined

---- Calculate matrix-vector product sums of Wab and impulses rs
wXr :: Acc (Wxxs) -> Acc (VectorList Double) -> Acc (Vector Double)
wXr wabs' rs' = fold (+) d0 $ reshape sh2x2 rowSum
  where
    rowSum   = fold (+) d0 products
    products = transpose $ reshape rsSh $ A.zipWith (*) wabs rs
    wabs     = flatten wabs'
    rs       = flatten $ A.replicate repSh rs'
    repSh    = lift $ Z :.All :.i2 :.All
    rsSh     = lift $ Z :. rows :.i4
    rows     = indexHead $ shape $ transpose rs'
    sh2x2    = lift $ Z :.i2 :.i2
    d0       = constant 0 :: Exp Double
    i2       = constant 2 :: Exp Int
    i4       = constant 4 :: Exp Int

---- Calculate vector addition
vadd :: (Elt e, IsNum e) => Acc (Vector e) -> Acc (Vector e) -> Acc (Vector e)
vadd a b = A.zipWith (+) a b
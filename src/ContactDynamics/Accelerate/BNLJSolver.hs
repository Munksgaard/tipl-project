module BNLJSolver where

import ContactDynamics.Disc
import ContactDynamics.Accelerate.Contact
import qualified Numeric.LinearAlgebra as M
import qualified Data.List as L
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as Backend

type VectorList e     = Array DIM2 e
type Wxx              = Array DIM2 Double
type Wxxs             = Array DIM3 Double
type Wxxss            = Array DIM4 Double

i0 = constant 0 :: Exp Int
i1 = constant 1 :: Exp Int
i2 = constant 2 :: Exp Int
i3 = constant 3 :: Exp Int
i4 = constant 4 :: Exp Int

d0 = constant 0.0 :: Exp Double
d1 = constant 1.0 :: Exp Double
d2 = constant 2.0 :: Exp Double
d3 = constant 3.0 :: Exp Double
d4 = constant 4.0 :: Exp Double

-- Int               = Number of iterations
-- [Disc]            = The discs in the scene
-- [M.Vector Double] = The external force, a list of vectors
-- VectorList Double = The resultant impulses on [Disc], a Nx2 vector-matrix
bnljSolver :: Int -> [Disc] -> [M.Vector Double] -> VectorList Double
bnljSolver maxIter ds extF = Backend.run (iter' step rs_init)
  where
    iter'                 = iter maxIter w'
    step                  = solveRHS' . calcRHS'
    solveRHS'             = solveRHS invs
    calcRHS'              = calcRHS n extF' wss
    extF'                 = (A.map (\x -> -x)
                             $ use
                             $ A.fromList fSh
                             $ L.concat
                             $ L.map M.toList extF)
    rs_init               = fill rSh d0
    rSh                   = lift (Z :.n :.i2)
    fSh                   = Z :.n' :.2
    w'                    = A.replicate repSh w
    repSh                 = lift $ Z :.All :.i2
    (n', n, w, invs, wss) = liftData ds

iter :: Int ->
        Acc (VectorList Double) ->
        (Acc (VectorList Double) -> Acc (VectorList Double)) ->
        Acc (VectorList Double) ->
        Acc (VectorList Double)
iter 0 _ _ rs = rs
iter n w solver rs = iter (n-1) w solver (rs'')
  where
    rs'' = A.zipWith (+)
           (A.zipWith (*) w rs')
           (A.zipWith(\x y -> (1-x) * y) w rs)
    rs'  = solver rs

---- Calculate right hand side
-- Exp Int                 = N: A constant that tells us the total number of contacts
-- Acc (VectorList Double) = The external force, a Nx2 vector-matrix
-- Acc (Wxxss)             = The Wab delassus operators for ALL RHS, a NxNx2x2 matrix where N is the number of contacts
-- Acc (VectorList Double) = The previously calculated impulses for each contact, a Nx2 vector-matrix
-- Acc (VectorList Double) = The right hand sides for all the contacts, a Nx2 vector-matrix
calcRHS :: Exp Int -> Acc (VectorList Double) -> Acc (Wxxss) -> Acc (VectorList Double) -> Acc (VectorList Double)
calcRHS n extF wabss rs = extF `vssub` (wabss `wXr'` rs)
  where
    wXr'  = wXr n

---- Solve right hand side
-- Acc (VectorList Double) = The inverse of the Waa for each contact,
--                           since Waa is a diagonal this is a Nx2 vector-matrix
-- Acc (VectorList Double) = The right hand side for each contact, a Nx2 vector-matrix
-- Acc (VectorList Double) = The resultant impulse for each contact, a Nx2 vector-matrix
solveRHS :: Acc (VectorList Double) -> Acc (VectorList Double) -> Acc (VectorList Double)
solveRHS inWaas rhss = A.zipWith (*) condM $ A.zipWith (*) inWaas rhss
  where
    condM   = A.replicate repSh $ A.map (A.fromIntegral . boolToInt . (>*0)) rhs1s
    rhs1s   = slice rhss sliceSh
    sliceSh = lift $ Z :.All :.i1
    repSh   = lift $ Z :.All :.i2

---- Calculate matrix-vector product sums of wabs and impulses for each contact AT THE SAME TIME
-- arg  1: N: A constant that tells us the total number of contacts
-- arg  2: This is where I start going insane.
--         N rows of N wabs that are each 2x2 for a total of a NxNx2x2 matrix in row major order,
--         ie. internally it goes first contact all wabs, second contact all wabs, etc. with all the dimensions
-- arg  3: A list of the previous impulses for each contact
-- result: a list of the matrix-vector product sums of wabs and impulses for each contact
wXr :: Exp(Int) -> Acc(Wxxss) -> Acc(VectorList Double) -> Acc(VectorList Double)
wXr n wabss' rs' = columnSums
  where
    columnSums = fold (+) d0 $ reshape nx2x2 rowSums
    rowSums    = fold (+) d0 $ reshape i4tnxnSh products
    products   = transpose $ reshape ntnxi4Sh $ A.zipWith (*) wabs rs
    wabs       = flatten wabss'
    rs         = flatten $ A.replicate repSh rs'
    repSh      = lift $ Z :.n      :.All    :.i2 :.All
    ntnxi4Sh   = lift $ Z :.(n*n)  :.i4
    i4tnxnSh   = lift $ Z :.(i4*n) :.n
    nx2x2      = lift $ Z :.n      :.i2 :.i2

---- Calculate vector-matrix addition
vssub :: (Elt e, IsNum e) => Acc (VectorList e) -> Acc (VectorList e) -> Acc (VectorList e)
vssub a b = A.zipWith (-) a b
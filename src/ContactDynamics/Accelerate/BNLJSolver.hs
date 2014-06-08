module BNLJSolver where

import ContactDynamics.Disc
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
-- VectorList Double = The external force, a Nx2 vector-matrix
-- VectorList Double = The resultant impulses on [Disc], a Nx2 vector-matrix
bnljSolver :: Int -> [Disc] -> VectorList Double -> VectorList Double
bnljSolver maxIter ds extF = Backend.run (iter' solve rs)
  where
    iter'       = iter maxIter
    solve       = solveRHS' . calcRHS'
    solveRHS'   = solveRHS inWass
    calcRHS'    = calcRHS numContacts extF' wabss
    extF'       = use extF
    inWass      = undefined
    wabss       = undefined
    numContacts = undefined
    rs          = undefined

iter :: Int -> (Acc (VectorList Double) -> Acc (VectorList Double)) -> Acc (VectorList Double) -> Acc (VectorList Double)
iter 0 _ rs = rs
iter n solve rs = iter (n-1) solve (solve rs)

---- Calculate right hand side
-- Exp Int                 = N: A constant that tells us the total number of contacts
-- Acc (VectorList Double) = The external force, a Nx2 vector-matrix
-- Acc (Wxxss)             = The Wab delassus operators for ALL RHS, a NxNx2x2 matrix where N is the number of contacts
-- Acc (VectorList Double) = The previously calculated impulses for each contact, a Nx2 vector-matrix
-- Acc (VectorList Double) = The right hand sides for all the contacts, a Nx2 vector-matrix
calcRHS :: Exp Int -> Acc (VectorList Double) -> Acc (Wxxss) -> Acc (VectorList Double) -> Acc (VectorList Double)
calcRHS n extF wabss rs = extF `vsadd` (wabss `wXr'` rs)
  where
    wXr' = wXr n

---- Solve right hand side
-- Acc (VectorList Double) = The inverse of the Waa for each contact,
--                           since Waa is a diagonal this is a Nx2 vector-matrix
-- Acc (VectorList Double) = The right hand side for each contact, a Nx2 vector-matrix
-- Acc (VectorList Double) = The resultant impulse for each contact, a Nx2 vector-matrix
solveRHS :: Acc (VectorList Double) -> Acc (VectorList Double) -> Acc (VectorList Double)
solveRHS inWaas rhss = undefined

-- solveRHS inWaa rhs = cond ?| (r', z0)
--   where
--     cond = rhs A.!! i1 >* d0
--     r' = A.zipWith (*) inWaa rhs
--     z0 = lift $ fromList (Z :. (2 :: Int)) [0,0]

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
vsadd :: (Elt e, IsNum e) => Acc (VectorList e) -> Acc (VectorList e) -> Acc (VectorList e)
vsadd a b = A.zipWith (+) a b
--------------------------------------------------------------------------
-- |
-- Module      :  Untyped.Calculus
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Calculus for the untyped system.
--
--------------------------------------------------------------------------
module Untyped.Calculus (
       -- * Data types
       Computation, Computer,
       -- * Evaluator
       eval1,
       -- * Evaluation methods
       compute, safecompute, bigstep,
       -- * Term predicates
       canStep, isStuck
       ) where
--------------------------------------------------------------------------
import Sidekick        ( failAll, showAll, protectWith )
import Untyped.Term    ( Term(..), isNatVal, isVal, (|->) )
--------------------------------------------------------------------------


-- | List of terms that represents a computation.
type Computation = [Term]

-- | A computer takes a term and returns the term's computation.
type Computer = Term -> Computation


{- Operational semantics. -}

-- | Small-step evaluation.
-- Operational semantics for this calculus.
eval1 :: Term -> Term
eval1 term = case term of

    -- unit
    TmUnit                      -> TmStop

    -- boolean
    TmTrue                      -> TmStop
    TmFalse                     -> TmStop

    TmIf TmTrue t1 _            -> t1
    TmIf TmFalse _ t2           -> t2
    TmIf t t1 t2
      | canStep t               -> TmIf t' t1 t2
      | otherwise               -> TmStop
      where t' = eval1 t

    -- arithmetic
    TmZero                      -> TmStop

    TmSucc t
      | canStep t               -> TmSucc t'
      | otherwise               -> TmStop
      where t' = eval1 t

    TmPred TmZero               -> TmZero
    TmPred t@(TmSucc nv)
      | canStep t               -> TmPred t'
      | isNatVal nv             -> nv
      | otherwise               -> TmStop
      where t' = eval1 t

    TmIsZero TmZero             -> TmTrue
    TmIsZero t@(TmSucc nv)
      | canStep t               -> TmIsZero t'
      | isNatVal nv             -> TmFalse
      | otherwise               -> TmStop
      where t' = eval1 t

    --- lambda calculus
    (TmAbs x t) `TmApp` t2      -> (x |-> t2) t
    t1 `TmApp` t2
      | canStep t1              -> t1' `TmApp` t2
      | isVal t1 && canStep t2  -> t1  `TmApp` t2'
      | otherwise               -> TmStop
      where [t1',t2'] = map eval1 [t1,t2]

    --- special
    TmLoop                      -> TmLoop
    _________________________   -> TmStop


{- Term predicates -}

-- | Checks if the given term can make a computational step.
canStep :: Term -> Bool
canStep t = case (eval1 t) of
    TmStop  -> False
    ______  -> True

-- | A term that cannot take any step, but still is not a value.
isStuck :: Term -> Bool
isStuck = failAll [canStep, isVal]


{- Evaluation methods -}

-- | Step-by-step evaluation.  It may easily loop infinitely.
compute :: Computer
compute = takeWhile (/= TmStop) . (iterate eval1)

-- | Step-by-step safe evaluation, avoiding easy-to-catch loops.
safecompute :: Computer
safecompute = (protectWith TmLoop [TmStop]) . (iterate eval1)

-- | Transforms a 'Computer' into a big-step evaluator.
bigstep :: Computer -> Term -> Term
bigstep = (last .)


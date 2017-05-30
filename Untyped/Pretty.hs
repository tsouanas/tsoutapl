--------------------------------------------------------------------------
-- |
-- Module      :  Untyped.Pretty
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Pretty-printers for the untyped system.
--
--------------------------------------------------------------------------
module Untyped.Pretty (
       -- * Pretty-printers
       prettyTerm, prettyC
       ) where
--------------------------------------------------------------------------
import Untyped.Term        ( Term(..), isNatVal )
import Untyped.Calculus    ( Computation )
import Sidekick            ( joinWith' )
--------------------------------------------------------------------------


-- | Pretty-printers.
type Pretty a = a -> String


-- | Pretty-print a 'Term'.
prettyTerm :: Term -> String
prettyTerm term = case term of
    TmUnit          -> "unit"
    TmTrue          -> "true"
    TmFalse         -> "false"
    TmIf t t1 t2    -> "(if " ++ s ++ " then " ++ s1 ++ " else " ++ s2 ++ ")" 
                       where [s,s1,s2] = map prettyTerm [t,t1,t2]
    TmZero          -> "0"
    TmSucc t
      | isNatVal t  -> (show . intFromNat) t
      | otherwise   -> "(succ " ++ prettyTerm t ++ ")"
    TmPred t        -> "(pred " ++ prettyTerm t ++ ")"
    TmIsZero t      -> "(iszero " ++ prettyTerm t ++ ")"
    TmVar x         -> "" ++ x ++ ""
    TmAbs x t       -> "(\\" ++ x ++ ". " ++ prettyTerm t ++ ")"
    t1 `TmApp` t2   -> "(" ++ prettyTerm t1 ++ " " ++ prettyTerm t2 ++ ")"
    TmStop          -> "$stop$"
    TmLoop          -> "@loop@"


-- | Turn a Nat to an Int.
intFromNat :: Term -> Int
intFromNat TmZero      = 0
intFromNat (TmSucc t)  = 1 + intFromNat t
intFromNat t           = error (show t ++ " is not a numeral")


-- | Pretty-print a 'Computation'.
prettyComputation :: String -> Pretty Term -> Computation -> String
prettyComputation sep termPrinter = (joinWith' "" "\n" sep) . map termPrinter

-- | Given a separator string, creates a 'Computation' pretty-printer.
prettyC :: String -> Pretty Computation
prettyC sep = prettyComputation sep prettyTerm


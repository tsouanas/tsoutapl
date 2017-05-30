--------------------------------------------------------------------------
-- |
-- Module      :  Typed.Pretty
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Pretty-printers for the typed system.
--
--------------------------------------------------------------------------
module Typed.Pretty (
       -- * Pretty-printers
       prettyTerm, prettyType, prettyTypedTerm, prettyC, prettyTC
       ) where
--------------------------------------------------------------------------
import {-# SOURCE #-} Typed.TypeSystem ( typeOf )
import Typed.Term          ( Term(..), isNatVal )
import Typed.Type          ( Type(..), Context )
import Typed.Calculus      ( Computation )
import Sidekick            ( joinWith' )
--------------------------------------------------------------------------


-- | Pretty-printers.
type Pretty a = a -> String


-- + Updated TmAbs to properly print the full 'Typing'.
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
    TmAbs (x,p) t   -> "(\\" ++ x ++ ":" ++ prettyType p ++ ". " ++ prettyTerm t ++ ")"
    t1 `TmApp` t2   -> "(" ++ prettyTerm t1 ++ " " ++ prettyTerm t2 ++ ")"
    TmWrong msg     -> "(wrong: " ++ msg ++ ")"
    TmStop          -> "$stop$"
    TmLoop          -> "@loop@"


-- + Added this new pretty-printer for Types.
-- | Pretty-print a 'Type'.
prettyType :: Type -> String
prettyType p = case p of
    TpUnit           -> "Unit"
    TpBool           -> "Bool"
    TpNat            -> "Nat"
    p1 `TpArrow` p2  -> "(" ++ prettyType p1 ++ " -> " ++ prettyType p2 ++ ")"
    TpCustom s       -> "`" ++ s ++ "`"
    TpBap s          -> "?" ++ s ++ "?"
    TpWrong          -> "Wrong"
    TpError s        -> "<Error: " ++ s ++ ">"


-- + A combination of the two previos pretty-printers.
-- | Pretty-print a 'Term' and its inferred 'Type'.
prettyTypedTerm :: Context -> Term -> String
prettyTypedTerm ctx t = prettyTerm t ++ " :: " ++ (prettyType $ typeOf ctx t)


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

-- + And the corresponding shortcut.
-- | Given a separator string and a 'Context', creates a 'Computation' pretty-printer.
prettyTC :: String -> Context -> Pretty Computation
prettyTC sep ctx = prettyComputation sep $ prettyTypedTerm ctx

--------------------------------------------------------------------------
-- |
-- Module      :  Typed.Term
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Terms of the typed system.
--
--------------------------------------------------------------------------
module Typed.Term ( 
       -- * Data types
         Term(..)
       , Program
       -- * Relations
       , (@=)
       -- * Subterms and variables
       , subterms
       , variables
       , freeVariables
       , freeIn
       , notFreeIn
       -- * Properties of terms
       , isUnitVal
       , isNatVal
       , isBoolVal
       , isAbsVal
       , isVal
       -- * Supplamentary creators
       , natFromInt
       , abstract
       , apply
       -- ** Substitutions
       , (|->)
       , (@>)
       ) where
--------------------------------------------------------------------------
import Typed.Type    ( Typing, baptize )
import Sidekick      ( passAny )
import List          ( union, (\\) )
--------------------------------------------------------------------------


-- + TmAbs requires a Typing instead of a plain String now.
-- + Added wrong terms to handle errors internally.
-- | Definition of terms for this calculus.
--
-- /Beware/: this, as an instance of @Eq@, never heard of the alpha-conversion.
-- Use @(\@=)@ if you need to check for alpha-equivalence.
data Term = TmUnit               -- ^ Unit value.
          | TmTrue               -- ^ Boolean value true.
          | TmFalse              -- ^ Boolean value false.
          | TmIf Term Term Term  -- ^ Branching.
          | TmZero               -- ^ Natural number zero.
          | TmSucc Term          -- ^ Successor.
          | TmPred Term          -- ^ Predecessor.
          | TmIsZero Term        -- ^ Zero test.
          | TmVar String         -- ^ Variable.
          | TmAbs Typing Term    -- ^ Lambda abstraction.
          | TmApp Term Term      -- ^ Term application.
          | TmWrong String       -- ^ A term representing an exception-like error.
          | TmStop               -- ^ Stop of evaluation meta-term.
          | TmLoop               -- ^ A term that loops infinitely.
          deriving (Eq, Show)    -- Beware, this Eq never heard of the alpha-conversion.
                                 -- Use (@=) if you need to check for alpha-equivalence.

-- | A list of terms that represent a program.
type Program = [Term]


-- | Turn an integral number to the corresponding 'Nat' term.
natFromInt :: Integral t => t -> Term
natFromInt 0        = TmZero
natFromInt (n + 1)  = TmSucc (natFromInt n)
natFromInt _        = error "Negative nat?!"


-- + We need to baptize the term now, as abstractions need 'Typing's, not Strings.
-- | Lambda-abstract a given variable term in a given term.
abstract :: Term -> Term -> Term
abstract (TmVar x) t  = TmAbs (x, baptize x) t
abstract _         _  = error "can only abstract variable terms"


-- | Turn a list of terms into a single application term.
-- (Like @foldr1 TmApp types@ but returns @unit@ if empty.)
apply :: [Term] -> Term
apply []  = TmUnit
apply ts  = foldl1 TmApp ts


{- Properties and relations -}

-- + Look into the typing, ignoring the actual type.
-- | alpha-conversion equivalence.
(@=) :: Term -> Term -> Bool
t @= s = if t == s then True else case (t, s) of 
    ( t1 `TmApp` t2  , s1 `TmApp` s2  )  -> t1 @= s1 && t2 @= s2
    ( TmSucc t'      , TmSucc s'      )  -> t' @= s'
    ( TmPred t'      , TmPred s'      )  -> t' @= s'
    ( TmIsZero t'    , TmPred s'      )  -> t' @= s'
    ( TmAbs (x,_) t' , TmAbs (y,_) s' )  -> t' @= (y @> x) s'
    ___________________________________  -> False


valueChecks :: [Term -> Bool]
valueChecks  = [isUnitVal, isBoolVal, isNatVal, isAbsVal]

-- | Checks if the given term is a unit value.
isUnitVal :: Term -> Bool
isUnitVal TmUnit     = True
isUnitVal _          = False

-- | Checks if the given term is a boolean value.
isBoolVal :: Term -> Bool
isBoolVal TmTrue     = True
isBoolVal TmFalse    = True
isBoolVal _          = False

-- | Checks if the given term is a natural number value.
isNatVal :: Term -> Bool
isNatVal TmZero      = True
isNatVal (TmSucc t)  = isNatVal t
isNatVal _           = False

-- | Checks if the given term is an abstraction value.
isAbsVal :: Term -> Bool
isAbsVal TmAbs {}    = True
isAbsVal _           = False

-- | Checks if the given term is a value.
isVal :: Term -> Bool
isVal  = passAny valueChecks


{- Variables -}

-- | List of all subterms of the given term.
subterms :: Term -> [Term]
subterms term = case term of
    TmVar x        -> [term]
    TmAbs _ t      -> term : subterms t
    t1 `TmApp` t2  -> term : (foldl1 union $ map subterms [t1, t2])
    TmIf t t1 t2   -> term : (foldl1 union $ map subterms [t, t1, t2])
    TmSucc t       -> term : subterms t
    TmPred t       -> term : subterms t
    TmIsZero t     -> term : subterms t
    _____________  -> [term]


-- | All variables that occur in the given term.
variables :: Term -> [String]
variables term = case term of
    TmVar x        -> [x]
    TmAbs _ t      -> variables t
    t1 `TmApp` t2  -> foldl1 union $ map variables [t1, t2]
    TmIf t t1 t2   -> foldl1 union $ map variables [t, t1, t2]
    TmSucc t       -> variables t
    TmPred t       -> variables t
    TmIsZero t     -> variables t
    _____________  -> []


-- + Take care of the typing in TmAbs, ignoring the actual type.
-- | All variables that occur /free/ in the given term.
freeVariables :: Term -> [String]
freeVariables term = case term of
    TmVar x        -> [x]
    TmAbs (x,_) t  -> (freeVariables t) \\ [x]
    t1 `TmApp` t2  -> foldl1 union $ map freeVariables [t1, t2]
    TmIf t t1 t2   -> foldl1 union $ map freeVariables [t, t1, t2]
    TmSucc t       -> freeVariables t
    TmPred t       -> freeVariables t
    TmIsZero t     -> freeVariables t
    _____________  -> []


-- | Handy shortcut for \"@x@ in FV(@t@)\".
freeIn :: String -> Term -> Bool
x `freeIn` t = x `elem` (freeVariables t)

-- | \"x not in FV(@t@)\".  What; this is handy too!
notFreeIn :: String -> Term -> Bool
x `notFreeIn` t = not (x `freeIn` t)


{- Variables names -}

niceNames :: [String]
niceNames = ["x", "y", "z"]

-- | A list of nice names for variables, using primes.
--
-- /BEWARE!/  Infinite list: @[x, y, z, x', y', z', x'', y'', z'', ...]@
niceNamesPrimed :: [String]
niceNamesPrimed = zipWith (++) names decorators
    where names      = cycle niceNames
          decorators = [ replicate n '\'' | n <- [0..]]

-- | A list of nice names for variables, using numeric indices.
--
-- /BEWARE!/  Infinite list: @[x0, y0, z0, x1, y1, z1, x2, y2, z2, ...]@
niceNamesIndexed :: [String]
niceNamesIndexed = zipWith (++) names decorators
    where names      = cycle niceNames
          decorators = map show [ n `div` (length niceNames) | n <- [0..] ]

-- | Something like a Hartogs operator for finding fresh variables.
hartogs :: [String] -> String
hartogs taken = head $ dropWhile (`elem` taken) niceNamesIndexed


{- Substitutions and all that Jazz -}

-- + Trivially adjust the TmAbs case to take into consideration the whole typing.
-- | Substitution in lambda-calculus terms.
--
-- @(x |-> s) t@ is the term we get if we replace every free occurrence
-- of @x@ in @t@ with @s@.
(|->) :: String -> Term -> Term -> Term

(x |-> s) term = case term of
    TmVar y              -> if x == y then s else term
    TmSucc t             -> TmSucc t'       where t' = (x |-> s) t
    TmPred t             -> TmPred t'       where t' = (x |-> s) t
    TmIsZero t           -> TmIsZero t'     where t' = (x |-> s) t
    t1 `TmApp` t2        -> t1' `TmApp` t2' where [t1', t2']     = map (x |-> s) [t1, t2]
    TmIf t t1 t2         -> TmIf t' t1' t2' where [t', t1', t2'] = map (x |-> s) [t, t1, t2]
    TmAbs c@(y,_) t
      | x == y           -> term
      | y `notFreeIn` s  -> TmAbs c ((x |-> s) t)
      | otherwise        -> (x |-> s) $ (y @> w) term
      where
        w        = hartogs usedvars
        usedvars = freeVariables s `union` variables t
    ___________________  -> term


-- + Trivially adjust the TmAbs case to take into consideration the whole typing.
-- | Renaming in the spirit of the alpha-conversion of lambda-calculus.
--
-- /BEWARE!/  This isn't safe: @w@ must be /correctly/ picked before-hand.
-- In fact, this is the most brain-dead substitution implementation.
(@>) :: String -> String -> Term -> Term
(x @> w) term = case term of
    TmSucc t         -> TmSucc t'        where t' = (x @> w) t
    TmPred t         -> TmPred t'        where t' = (x @> w) t
    TmIsZero t       -> TmIsZero t'      where t' = (x @> w) t
    t1 `TmApp` t2    -> t1' `TmApp` t2'  where [t1', t2']     = map (x @> w) [t1, t2]
    TmIf t t1 t2     -> TmIf t' t1' t2'  where [t', t1', t2'] = map (x @> w) [t, t1, t2]
    TmVar y          -> if (x == y) then (TmVar w) else term
    TmAbs c@(y,p) t  -> if (x == y) then (TmAbs (w,p) t') else (TmAbs c t')
                        where t' = (x @> w) t
    _____________    -> term


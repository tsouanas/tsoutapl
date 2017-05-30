--------------------------------------------------------------------------
-- |
-- Module      :  Typed.TypeSystem
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Type system of the typed system.
--
--------------------------------------------------------------------------
module Typed.TypeSystem (
         typeOf
       , appType
       , ifType
       ) where
--------------------------------------------------------------------------
import Typed.Pretty  ( prettyTerm, prettyType )
import Typed.Term    ( Term(..) )
import Typed.Type    ( Type(..), Context, baptize, dom, codom )
import Maybe         ( fromMaybe )
--------------------------------------------------------------------------


-- | Tries to type the given 'Term', w.r.t. the given 'Context'.
typeOf :: Context -> Term -> Type
typeOf ctx term = case term of

    -- unit
    TmUnit                     -> TpUnit

    -- booleans
    TmTrue                     -> TpBool
    TmFalse                    -> TpBool
    TmIsZero t
      | typeOf ctx t == TpNat  -> TpBool
      | otherwise              -> TpError "only numbers can be tested for zeroness"

    -- arithmetic
    TmZero                     -> TpNat
    TmSucc t
      | typeOf ctx t == TpNat  -> TpNat
      | otherwise              -> TpError "can only apply succ to numbers"
    TmPred t
      | typeOf ctx t == TpNat  -> TpNat
      | otherwise              -> TpError "can only apply pred to numbers"

    -- lambda calculus
    TmVar x                    -> fromMaybe (baptize x) (lookup x ctx)
    TmAbs r@(_,p) t            -> p `TpArrow` (typeOf (r:ctx) t)
    t1 `TmApp` t2              -> appType p1 p2   where [p1,p2]    = map (typeOf ctx) [t1,t2]
    TmIf t t1 t2               -> ifType p p1 p2  where [p,p1,p2]  = map (typeOf ctx) [t,t1,t2]

    -- special
    TmLoop                     -> TpError "BOTTOM"
    TmWrong _                  -> TpWrong
    _________________________  -> TpError $ "cannot type term " ++ prettyTerm term


{- Additional typers which use pretty-printing -}

-- | Try to type two given types as an application.
appType :: Type -> Type -> Type
appType p1 p2 = case (dom p1) of
    Nothing            -> TpError $ prettyType p1 ++ " is not arrow-ish"
    Just p1dom
        | p1dom /= p2  -> TpError $ "arrow " ++ prettyType p1
                          ++ " should have domain " ++ prettyType p2
        | otherwise    -> fromMaybe
                              (TpError $ "cannot infer the codomain of " ++ prettyType p1)
                              (codom p1)

-- | Try to type three given types as a branching.
ifType :: Type -> Type -> Type -> Type
ifType p p1 p2
    | p /= TpBool  = TpError "predicate is not boolean"
    | p1 == p2     = p1
    | otherwise    = TpError $ "either " ++ prettyType p1 ++ " or " ++ prettyType p2


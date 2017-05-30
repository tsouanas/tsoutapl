--------------------------------------------------------------------------
-- |
-- Module      :  Typed.Type
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Types of the typed system.
--
--------------------------------------------------------------------------
module Typed.Type ( 
       -- * Data types
         Type(..)
       , Typing
       , Context
       -- * Supplamentary creators and extractors
       , baptize
       , dom
       , codom
       , context
       , arrowize
       ) where
--------------------------------------------------------------------------
import Maybe           ( fromMaybe )
import Char            ( toUpper )
--------------------------------------------------------------------------


-- | Definition of types for this calculus.
data Type = TpUnit               -- ^ Unit type.
          | TpBool               -- ^ Boolean type.
          | TpNat                -- ^ Natural numbers type.
          | TpArrow Type Type    -- ^ Function type.
          | TpCustom String      -- ^ Custom-named type.
          | TpBap String         -- ^ Baptized type.
          | TpWrong              -- ^ Wrong type, with an explanation of what went wrong.
          | TpError String       -- ^ Type error.
          deriving (Eq, Show)

-- | A typing of a variable name to a type.
type Typing = (String, Type)

-- | A series of typings.
type Context = [Typing]


{- Helper typers and creators -}

-- | Make up a type name for the given variable.
baptize :: String -> Type
baptize = TpBap . map toUpper

-- | Return /just/ the domain of an arrow.
-- If the type has been made up, make up another one for its domain.
dom :: Type -> Maybe Type
dom (p1 `TpArrow` _)  = Just p1
dom (TpBap p)         = Just $ TpBap $ "dom(" ++ p ++ ")"
dom _                 = Nothing

-- | Return /just/ the codomain of an arrow.
-- If the type has been made up, make upanother one for its domain.
codom :: Type -> Maybe Type
codom (_ `TpArrow` p2)  = Just p2
codom (TpBap p)         = Just $ TpBap $ "codom(" ++ p ++ ")"
codom _                 = Nothing

-- | Type many variables at once with a single Type, returning a Context.
context :: [String] -> Type -> Context
context xs p  = map (\x -> (x,p)) xs

-- | Turn a list of types into an arrow by associating to the right.
-- (Something like foldr1 TpArrow types but returns Unit if empty.)
arrowize :: [Type] -> Type
arrowize []      = TpUnit
arrowize [p]     = p
arrowize ps      = foldr1 TpArrow ps


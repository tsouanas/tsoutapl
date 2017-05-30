--------------------------------------------------------------------------
-- |
-- Module      :  Typed
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Exporter module for the typed system.
--
--------------------------------------------------------------------------
module Typed (
         interpret
       , biginterpret
       , quietinterpret
       , compute
       , safecompute
       ) where
--------------------------------------------------------------------------
import Typed.Term
import Typed.Calculus
import Typed.Pretty
import Typed.Parser
import Typed.Interpreter
import Typed.Type
import Typed.TypeSystem
--------------------------------------------------------------------------


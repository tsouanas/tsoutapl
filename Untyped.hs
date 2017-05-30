--------------------------------------------------------------------------
-- |
-- Module      :  Untyped
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Exporter module for the untyped system.
--
--------------------------------------------------------------------------
module Untyped (
         interpret
       , biginterpret
       , quietinterpret
       , compute
       , safecompute
       ) where
--------------------------------------------------------------------------
import Untyped.Term
import Untyped.Calculus
import Untyped.Pretty
import Untyped.Parser
import Untyped.Interpreter
--------------------------------------------------------------------------


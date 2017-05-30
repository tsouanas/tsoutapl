--------------------------------------------------------------------------
-- |
-- Module      :  Untyped.Interpreter
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Interpreters for the untyped system.
-- Links the parser and the evaluator, handling the dirty stuff.
--
--------------------------------------------------------------------------
module Untyped.Interpreter (
       -- * Interpreters
       interpret, biginterpret, quietinterpret
       ) where
--------------------------------------------------------------------------
import Untyped.Calculus    ( Computer, bigstep )
import Untyped.Pretty      ( prettyC, prettyTerm )
import Untyped.Parser      ( parseProgram )
import Sidekick            ( showAll )
--------------------------------------------------------------------------


-- | Given a 'Computer' and a source code, return the detailed computations.
interpret :: Computer -> String -> String
interpret computer = showAll (prettyC "\n==> ")
                   . map computer
                   . parseProgram

-- | Given a 'Computer' and a source code, return only the results.
quietinterpret :: Computer -> String -> String
quietinterpret computer = showAll prettyTerm
                        . map (bigstep computer)
                        . parseProgram

-- | Given a 'Computer' and a source code, return the big-step computations.
biginterpret :: Computer -> String -> String
biginterpret computer = showAll (prettyC "\n==>* ")
                      . map (\t -> [t, (bigstep computer) t])
                      . parseProgram


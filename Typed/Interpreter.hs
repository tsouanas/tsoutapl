--------------------------------------------------------------------------
-- |
-- Module      :  Typed.Interpreter
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Interpreters for the typed system.
-- Links the parser and the evaluator, handling the dirty stuff.
--
--------------------------------------------------------------------------
module Typed.Interpreter (
       -- * Interpreters
       interpret, biginterpret, quietinterpret
       ) where
--------------------------------------------------------------------------
import Typed.Calculus    ( Computer, bigstep )
import Typed.Pretty      ( prettyTC, prettyTypedTerm )
import Typed.Parser      ( parseProgram, parseContext )
import Sidekick          ( showAll )
--------------------------------------------------------------------------


-- | Given a 'Computer', a context source and a program source,
-- return the detailed computations.
interpret :: Computer -> String -> String -> String
interpret computer ctx = showAll (prettyTC "\n==> " $ parseContext ctx)
                       . map computer
                       . parseProgram

-- | Given a 'Computer', a context source and a program source,
-- return only the results.
quietinterpret :: Computer -> String -> String -> String
quietinterpret computer ctx = showAll ((prettyTypedTerm . parseContext) ctx)
                            . map (bigstep computer)
                            . parseProgram

-- | Given a 'Computer', a context source and a program source,
-- return the big-step computations.
biginterpret :: Computer -> String -> String -> String
biginterpret computer ctx = showAll (prettyTC "\n==>* " $ parseContext ctx)
                          . map (\t -> [t, (bigstep computer) t])
                          . parseProgram


{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------
-- |
-- Module      :  Commander
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Command-line processing for TsouTAPL.
--
--------------------------------------------------------------------------
module Commander ( 
      TsouTAPL(..)
    , processArgs
    ) where
--------------------------------------------------------------------------
import System.Console.CmdArgs
--------------------------------------------------------------------------


-- | Different modes of TsouTAPL.
data TsouTAPL = Untyped { prgfile :: FilePath
                        , unprotected :: Bool
                        }
              | Typed   { prgfile :: FilePath
                        , ctxfile :: [FilePath]
                        , unprotected :: Bool
                        }
              deriving (Show, Data, Typeable)

-- Constant messages.
version      = "2.6"
welcomeMsg   = "TsouTAPL v" ++ version ++ ", by Thanos Tsouanas <thanos@sians.org>."


-- | Shortcut to process command-line arguments.
processArgs :: IO TsouTAPL
processArgs  = cmdArgs welcomeMsg $ map mode [untyped, typed]


{- Mode option definitions -}

-- Untyped system.
untyped  = Untyped
    { prgfile      = def &= argPos 0 & typ "PROGRAM_FILE"
    , unprotected  = def &= text "Unsafe mode: no protection from obvious infinite loops."
    } &= text "Untyped lambda calculus with unit, booleans and arithmetic."


-- Typed system.
typed  = Typed
    { prgfile      = def &= argPos 0 & typ "PROGRAM_FILE"
    , ctxfile      = def &= explicit & flag "c" & flag "context" & typ "CONTEXT_FILE"
                            & text "File containing pure context."
    , unprotected  = def &= text "Unsafe mode: no protection from obvious infinite loops."
    } &= text "Typed lambda calculus with unit, booleans, arithmetic and wrong."


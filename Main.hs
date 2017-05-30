{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- TsouTAPL: implementations of various lambda calculus based systems.
--
--------------------------------------------------------------------------
module Main ( main ) where
--------------------------------------------------------------------------
import qualified Untyped 
import qualified Typed
import Commander
import System.Console.CmdArgs    ( isQuiet, isNormal, isLoud )
--------------------------------------------------------------------------


-- | Gets the command-line arguments and runs in the appropriate mode.
main :: IO ()
main = do tsoutapl <- processArgs
          case tsoutapl of
               opt@Untyped {} -> runUntyped opt
               opt@Typed   {} -> runTyped opt


{- Different modes -}

-- Untyped lambda calculus with unit, booleans and arithmetic.
runUntyped :: TsouTAPL -> IO ()
runUntyped opt = do prgFile <- readFile $ prgfile opt
                    quiet   <- isQuiet
                    verbose <- isLoud
                    normal  <- isNormal
                    let interpreter | verbose          = Untyped.interpret
                                    | normal           = Untyped.biginterpret
                                    | quiet            = Untyped.quietinterpret
                    let computer    | unprotected opt  = Untyped.compute
                                    | otherwise        = Untyped.safecompute
                    putStrLn $ interpreter computer prgFile


-- Simply typed lambda calculus with unit, booleans, arithmetic and wrong.
runTyped :: TsouTAPL -> IO ()
runTyped opt  = do prgFile <- readFile $ prgfile opt
                   ctxFile <- if null (ctxfile opt)
                                 then do {return ""}
                                 else do {readFile $ head (ctxfile opt)}
                   quiet   <- isQuiet
                   verbose <- isLoud
                   normal  <- isNormal
                   let interpreter | verbose          = Typed.interpret
                                   | normal           = Typed.biginterpret
                                   | quiet            = Typed.quietinterpret
                   let computer    | unprotected opt  = Typed.compute
                                   | otherwise        = Typed.safecompute
                   putStrLn $ interpreter computer ctxFile prgFile

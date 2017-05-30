--------------------------------------------------------------------------
-- |
-- Module      :  Untyped.Parser
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Parsers for the Untyped system.
--
--------------------------------------------------------------------------
module Untyped.Parser ( 
       -- * Parsers
       parseProgram
       ) where
--------------------------------------------------------------------------
import Untyped.Term     ( Term(..), Program, natFromInt )
import Control.Monad    ( MonadPlus(..), ap, liftM )
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>) )
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
--------------------------------------------------------------------------


{- XXX: Comment these out if using ghc 6.12.* -}
 
instance Applicative (GenParser s a) where
    pure   = return
    (<*>)  = ap

instance Alternative (GenParser s a) where
    empty  = mzero
    (<|>)  = mplus


{- Top-level parsers -}

-- | Parse source-code from a string to a 'Program'.
parseProgram :: String -> Program
parseProgram input = case (parse parseprogram "(program parser)" input) of
                         Left e    -> (error . show) e
                         Right ts  -> ts


{- Language definition -}

prgLang  = LanguageDef
         { commentStart     = "[:"
         , commentEnd       = ":]"
         , commentLine      = "%"
         , nestedComments   = True
         , identStart       = lower
         , identLetter      = alphaNum <|> char '_' <|> (char '\'' <* notFollowedBy alphaNum)
         , opStart          = fail "operators not supported"
         , opLetter         = fail "operators not supported"
         , reservedNames    = [ "unit"
                              , "true"
                              , "false"
                              , "if"
                              , "then"
                              , "else"
                              , "zero"
                              , "succ"
                              , "pred"
                              , "iszero"
                              , "lambda"
                              ]
         , reservedOpNames  = []
         , caseSensitive    = True
         }

prg = makeTokenParser prgLang

{- XXX: Uncomment this, if running ghc 6.12.* (WEIRD hack)
 - blankline  =  try (semi prg)
 -}


{- High level parsers -}

parseprogram    = whiteSpace prg *> manyTill parsestatement eof
parsestatement  = parseterm <* (semi prg)
parseterm       = chainl1 parsenoapp $ return TmApp


{- Low-level parsers -}

parsenoapp  =  parseunitval
           <|> parseboolval
           <|> parsenatval
           <|> parseabsval
           <|> parsesucc
           <|> parsepred
           <|> parseiszero
           <|> parsevar
           <|> parseif
           <|> parens prg parseterm

parseunitval  = parseunit <?> "unit"
parseboolval  = parsetrue <|> parsefalse <?> "boolean"
parsenatval   = parsezero <|> parsenumber <?> "natural"
parseabsval   = parseabs

-- constant symbols
parseunit     = parse0ary "unit"   TmUnit
parsetrue     = parse0ary "true"   TmTrue
parsefalse    = parse0ary "false"  TmFalse
parsezero     = parse0ary "zero"   TmZero

-- unary symbols
parsesucc     = parse1ary "succ"   TmSucc
parsepred     = parse1ary "pred"   TmPred
parseiszero   = parse1ary "iszero" TmIsZero
parsenumber   = liftM natFromInt (natural prg) <?> "number constant"

-- variables
parsevar = do name <- identifier prg
              return $ TmVar name

-- branching
parseif = do reserved prg "if"
             t <- parseterm
             reserved prg "then"
             t1 <- parseterm
             reserved prg "else"
             liftM (TmIf t t1) parseterm

-- untyped lambda abstraction
parseabs = do symbol prg "\\"
              x <- identifier prg
              dot prg
              t <- parseterm
              return $ TmAbs x t


{- Shortcut parsers -}

parse0ary name cons = reserved prg name >> return cons
parse1ary name cons = reserved prg name >> liftM cons parseterm


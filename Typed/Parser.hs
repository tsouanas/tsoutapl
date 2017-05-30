--------------------------------------------------------------------------
-- |
-- Module      :  Typed.Parser
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- Parsers for the Typed system.
--
--------------------------------------------------------------------------
module Typed.Parser ( 
       -- * Parsers
       parseProgram, parseContext    -- + Another parser for contexts
       ) where
--------------------------------------------------------------------------
import Typed.Term       ( Term(..), Program, natFromInt )
import Typed.Type       ( Type(..), Context, context )
import Typed.Pretty     ( prettyTerm )
import Sidekick         ( functionize )
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

-- + Context parser.
-- | Parse source-code from a string to a 'Context'.
parseContext :: String -> Context
parseContext input = case (parse parsecontexts "(context parser)" input) of
                          Left e      -> (error . show) e
                          Right ctxs  -> (functionize . concat) ctxs


{- Language definition -}

-- + Separated prgLang into two languages: typesLang and termsLang.
typesLang  = LanguageDef
         { commentStart     = "[:"
         , commentEnd       = ":]"
         , commentLine      = "%"
         , nestedComments   = True
         , identStart       = upper
         , identLetter      = alphaNum <|> char '_' <|> (char '\'' <* notFollowedBy alphaNum)
         , opStart          = fail "operators not supported"
         , opLetter         = fail "operators not supported"
         , reservedNames    = [ "Unit"
                              , "Bool"
                              , "Nat"
                              , "Wrong"
                              ]
         , reservedOpNames  = ["->"]
         , caseSensitive    = True
         }

types = makeTokenParser typesLang

-- + Added wrong as a reserved word.
termsLang  = LanguageDef
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
                              , "wrong"
                              , "assert"
                              ]
         , reservedOpNames  = []
         , caseSensitive    = True
         }

terms = makeTokenParser termsLang

{- XXX: Uncomment this, if running ghc 6.12.* (WEIRD hack)
 - blankline  =  try (semi terms)
 -}


{- High level parsers -}

parseprogram    = whiteSpace terms *> manyTill parsestatement eof
parsestatement  = parseterm <* (semi terms)
parsetype       = chainr1 parsenoarrow $ reservedOp types "->" >> return TpArrow
parseterm       = chainl1 parsenoapp $ return TmApp

-- + Context and typings-statements high level parses.
parsecontexts   = whiteSpace terms *> manyTill parsetypings eof
parsetypings    = do xs <- sepBy (identifier terms) (comma terms)
                     symbol types "::"
                     p <- (parsetype <* symbol types ";")
                     return $ context xs p


{- Low-level parsers -}

-- + We type no-arrow types just like we type no-app terms...
parsenoarrow  =  parsetpunit
             <|> parsetpbool
             <|> parsetpnat
             <|> parsetpcustom
             <|> parens types parsetype

-- + ...and the base types are as simple as that!
parsetpunit    = parse0ary types "Unit" TpUnit
parsetpbool    = parse0ary types "Bool" TpBool
parsetpnat     = parse0ary types "Nat"  TpNat
parsetpcustom  = do name <- identifier types
                    return $ TpCustom name

parsenoapp  =  parseunitval
           <|> parseboolval
           <|> parsenatval
           <|> parseabsval
           <|> parsewrong
           <|> parseassert
           <|> parsesucc
           <|> parsepred
           <|> parseiszero
           <|> parsevar
           <|> parseif
           <|> parens terms parseterm

parseunitval  = parseunit <?> "unit"
parseboolval  = parsetrue <|> parsefalse <?> "boolean"
parsenatval   = parsezero <|> parsenumber <?> "natural"
parseabsval   = parseabs

-- constant symbols
parseunit     = parse0ary terms "unit"   TmUnit
parsetrue     = parse0ary terms "true"   TmTrue
parsefalse    = parse0ary terms "false"  TmFalse
parsezero     = parse0ary terms "zero"   TmZero

-- unary symbols
parsesucc     = parse1ary terms "succ"   TmSucc
parsepred     = parse1ary terms "pred"   TmPred
parseiszero   = parse1ary terms "iszero" TmIsZero
parsenumber   = liftM natFromInt (natural terms) <?> "number constant"

-- wrong
parsewrong = do reserved terms "wrong"
                msg <- stringLiteral terms
                return $ TmWrong msg

-- variables
parsevar = do name <- identifier terms
              return $ TmVar name

-- branching
parseif = do reserved terms "if"
             t <- parseterm
             reserved terms "then"
             t1 <- parseterm
             reserved terms "else"
             liftM (TmIf t t1) parseterm

-- untyped lambda abstraction
-- + This has to parse the typing now.
parseabs = do symbol terms "\\"
              x <- identifier terms
              colon terms
              p <- parsetype
              dot terms
              t <- parseterm
              return $ TmAbs (x,p) t

parseassert = do reserved terms "assert"
                 assertion <- parens terms parseterm
                 msg <- stringLiteral terms
                 return $ TmIf assertion TmUnit (TmWrong msg)

{- Shortcut parsers -}

-- + Now that we have two languages it's better to make token parser a parameter.
parse0ary tkp name cons = reserved tkp name >> return cons
parse1ary tkp name cons = reserved tkp name >> liftM cons parseterm


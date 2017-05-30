--------------------------------------------------------------------------
-- |
-- Module      :  Sidekick
-- Copyright   :  (c) Thanos Tsouanas 2010
-- License     :  BSD
-- Maintainer  :  thanos@sians.org
-- Stability   :  experimental
--
-- General-purpose helper functions.
--
--------------------------------------------------------------------------
module Sidekick (
    -- * List processing
      takeWhileUnique
    , joinWith
    , joinWith'
    , functionize
    , headfeet
    , protectWith
    -- * Pretty-printing
    , showAll
    -- * Boolean testing
    , passAny
    , passAll
    , failAny
    , failAll
    ) where 
--------------------------------------------------------------------------
import Maybe    ( fromMaybe )
--------------------------------------------------------------------------


-- | Keep taking until you reach two consequtive same values.
takeWhileUnique :: Eq a => [a] -> [a]
takeWhileUnique []         = []
takeWhileUnique [x]        = [x]
takeWhileUnique (x1:x2:xs)
    | x1 == x2  = [x1]
    | otherwise = x1 : takeWhileUnique (x2:xs)


-- | Try to find a fixpoint of the given function by
-- iterations starting from the given value.
fix :: Eq a => (a -> a) -> a -> a
fix f = last . takeWhileUnique . (iterate f)


-- | Join a list of lists together, using another list as a separator.
-- Like python's @sep.join(list)@.
joinWith :: [a] -> [[a]] -> [a]
joinWith sep []     = []
joinWith sep [x]    = x
joinWith sep (x:xs) = x ++ sep ++ (joinWith sep xs)

-- | Like 'joinWith', but it accepts start and end terms.
joinWith' :: [a] -> [a] -> [a] -> [[a]] -> [a]
joinWith' a z sep xs  =  a ++ joinWith sep xs ++ z

-- | Display each showable item of a list on a separate line.
showAll :: Show a => (a -> String) -> [a] -> String
showAll shower = unlines . map shower


-- | This is a smarter version of 'takeWhileUnique', in the sense
-- that it looks all the way back to find a repetition, while
-- 'takeWhileUnique' only looks at the previous value.
--
-- e.g.
-- @takeWhileUnique [0, 1, 2, 0, 1, 2, ...]@ is bottom,
-- while 'protectWith' would return @[0, 1, 2, guard]@.
protectWith :: Eq a => a -> [a] -> [a] -> [a]
protectWith = protectWith' []
    where
        protectWith' :: Eq a => [a] -> a -> [a] -> [a] -> [a]
        protectWith' trace guard terminal []      = []
        protectWith' trace guard terminal (x:xs)    
            | x `elem` terminal  = []
            | x `elem` trace     = [x, guard]
            | otherwise          = x : protectWith' (x:trace) guard terminal xs


-- | From a given list of tests, check if a given element satisfies any of them.
passAny :: [a -> Bool] -> a -> Bool
passAny = (or .) . testResults

-- | From a given list of tests, check if a given element satisfies all of them.
passAll :: [a -> Bool] -> a -> Bool
passAll = (and .) . testResults

-- | From a given list of tests, check if a given element fails all of them.
failAll :: [a -> Bool] -> a -> Bool
failAll = (not .) . passAny

-- | From a given list of tests, check if a given element fails any of them.
failAny :: [a -> Bool] -> a -> Bool
failAny = (not .) . passAll

-- | Perform a series of given tests on a given element and return the results.
testResults :: [a -> Bool] -> a -> [Bool]
testResults tests t = [ test t | test <- tests ]


-- | Get rid of the body of a list.
headfeet :: [a] -> [a]
headfeet []    = []
headfeet [x]   = [x]
headfeet [x,y] = [x,y]
headfeet xs    = [head xs, last xs]


-- | Turn a list of pairs into a function, where where later values overwrite
-- earlier ones.
functionize []            = []
functionize (p@(k,_):ps)  = case lookup k ps of
    Just v'  -> functionize ps
    _______  -> p : functionize ps


{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes
import LExp


dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma (DivL a b) = dec_rinv (a : gamma) b
dec_rinv gamma (DivR b a) = dec_rinv (gamma ++ [a]) b
-- Rule F: try every possible focus
dec_rinv gamma (Atm p) =
    or [dec_lfoc gammaL a gammaR p | i <- [0 .. length gamma - 1], let (gammaL, a : gammaR) = splitAt i gamma]


dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL (Atm q) gammaR p = null gammaL && null gammaR && q == p
dec_lfoc gammaL (DivL a b) gammaR p =
    or [dec_rinv gammaL2 a && dec_lfoc gammaL1 b gammaR p 
        | i <- [0 .. length gammaL], let (gammaL1, gammaL2) = splitAt i gammaL]
dec_lfoc gammaL (DivR b a) gammaR p =
    or [dec_rinv gammaR1 a && dec_lfoc gammaL b gammaR2 p 
        | i <- [0 .. length gammaR], let (gammaR1, gammaR2) = splitAt i gammaR]


isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws =
    let 
        -- Find all possible grammatical types for a given word
        wordTypes w = [tp | (w', tp, _) <- items lex, w' == w]
        -- Get the possible grammatical types for each word
        possibleTypes = [wordTypes w | w <- ws]
        -- Generate all combinations by choosing one type per word
        combinations = sequence possibleTypes
        -- Check whether each combination produces the target sentence type
        results = [dec_rinv ts (stp lex) | ts <- combinations]
    -- The sentence is grammatical if at least one combination succeeds
    in or results

-- Constructing all possible lambda expressions for a derivation
der_rinv :: [(Var, LTp)] -> LTp -> [LExp]
-- Rule R: introduce a new variable on the left
der_rinv gamma (DivL a b) =
    [L x e | e <- der_rinv ((x, a) : gamma) b]
    where x = fresh [V v | (v, _) <- gamma]

-- Rule R': introduce a new variable on the right
der_rinv gamma (DivR b a) =
    [L x e | e <- der_rinv (gamma ++ [(x, a)]) b]
    where x = fresh [V v | (v, _) <- gamma]

-- Rule F: try every possible focus and construct the expression
der_rinv gamma (Atm p) =
    [foldl A (V x) es | i <- [0 .. length gamma - 1]
        , let (gammaL, (x, a) : gammaR) = splitAt i gamma
        , es <- der_lfoc gammaL a gammaR p]


-- Find all possible argument lists for a focused derivation
der_lfoc :: [(Var, LTp)] -> LTp -> [(Var, LTp)] -> Atm -> [[LExp]]
-- Rule X: succeed if the types match and no other types remain
der_lfoc gammaL (Atm q) gammaR p =
    if null gammaL && null gammaR && q == p
    then [[]]
    else []

-- Rule L: derive an argument from the left and continue
der_lfoc gammaL (DivL a b) gammaR p =
    [e : es | i <- [0 .. length gammaL]
        , let (gammaL1, gammaL2) = splitAt i gammaL
        , e <- der_rinv gammaL2 a
        , es <- der_lfoc gammaL1 b gammaR p]

-- Rule L': derive an argument from the right and continue
der_lfoc gammaL (DivR b a) gammaR p =
    [e : es | i <- [0 .. length gammaR]
        , let (gammaR1, gammaR2) = splitAt i gammaR
        , e <- der_rinv gammaR1 a
        , es <- der_lfoc gammaL b gammaR2 p]
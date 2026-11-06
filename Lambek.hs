{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

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


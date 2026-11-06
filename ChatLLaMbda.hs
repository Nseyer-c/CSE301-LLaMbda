import Data.Char
import System.IO
import Control.Exception

import LTypes
import qualified Lambek
import LExp

import Parser
import PySupport

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

-- read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do
  -- print the prompt
  putStr "> "
  -- get a line of input
  line <- getLine
  -- split it up into "standardized" words
  let ws = map stdize $ words line
  
  {-
  -- test if the string of words is a grammatical sentence according to the lexicon
  if Lambek.isGrammatical elx ws
    then putStrLn "That is a well-formed sentence!"
    else putStrLn "That is not a well-formed sentence"
  -}

  -- Find all possible grammatical types for each numbered word
  let wordTypes (w, i) = [(w ++ "_" ++ show i, tp) | (w', tp, _) <- items elx, w' == w]
      -- Get the possible types for each word
      possibleTypes = [wordTypes (w, i) | (w, i) <- zip ws [1..]]
      -- Generate all combinations of grammatical types
      combinations = sequence possibleTypes
      -- Construct all possible lambda expressions
      expressions = concat [Lambek.der_rinv ts (stp elx) | ts <- combinations]

  -- Display the resulting lambda expressions
  if null expressions
    then putStrLn "That is not a well-formed sentence"
    else do
      putStrLn "I can derive your sentence as follows:"
      mapM_ printLExp expressions

  -- repeat
  repl elx phandle
      
main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython db
  -- start the read-eval-print loop
  putStrLn "Starting chat..."
  repl elx phandle
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)
      
    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex

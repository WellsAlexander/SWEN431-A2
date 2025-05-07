{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Control.Exception (try, SomeException, evaluate)

-- Stack element data type to represent all possible values
data StackElement
  = IntVal Int
  | BoolVal Bool
  | StrVal String
  | QuotedVal String
  deriving (Eq)

-- Custom Show instance for StackElement
instance Show StackElement where
  show (IntVal n) = show n
  show (BoolVal True) = "true"
  show (BoolVal False) = "false"
  show (StrVal s) = s
  show (QuotedVal s) = s

-- Parse an input token into a StackElement
parseToken :: String -> StackElement
parseToken token
  | all isDigit token = IntVal (read token)
  | isStringLiteral token = StrVal (init (tail token))  -- Remove quotes
  | token == "true" = BoolVal True
  | token == "false" = BoolVal False
  | head token == '\'' = QuotedVal (tail token)  -- Handle quoted elements
  | otherwise = parseOther token

-- Check if a token is a string literal (surrounded by quotes)
isStringLiteral :: String -> Bool
isStringLiteral ('"':rest) = last rest == '"' && not (null rest)
isStringLiteral _ = False

-- Parse operators and other elements
parseOther :: String -> StackElement
parseOther = QuotedVal

-- Function to process elements and maintain the stack
processStack :: [String] -> [StackElement] -> [StackElement]
processStack [] stack = stack
processStack (token:tokens) stack = case token of
  -- Basic arithmetic operators (Core)
  "+" -> processIntOp (+) tokens stack
  "-" -> processIntOp (-) tokens stack
  "*" -> processIntOp (*) tokens stack
  "/" -> processIntOp div tokens stack
  "%" -> processIntOp mod tokens stack
  "**" -> processIntOp (^) tokens stack

  -- Stack manipulation (Core)
  "DROP" -> processDrop tokens stack
  "DUP" -> processDup tokens stack
  "SWAP" -> processSwap tokens stack

  -- Additional stack operations (Completion)
  "ROT" -> processRot tokens stack
  "ROLL" -> processRoll tokens stack
  "ROLLD" -> processRolld tokens stack

  -- Comparison operators (Completion)
  "==" -> processCompOp (==) tokens stack
  "!=" -> processCompOp (/=) tokens stack
  ">" -> processCompOp (>) tokens stack
  "<" -> processCompOp (<) tokens stack
  ">=" -> processCompOp (>=) tokens stack
  "<=" -> processCompOp (<=) tokens stack
  "<=>" -> processSpaceship tokens stack

  -- Boolean operators (Completion)
  "&" -> processBoolOp (&&) tokens stack
  "|" -> processBoolOp (||) tokens stack
  "^" -> processBoolXor tokens stack

  -- Control flow (Completion)
  "IFELSE" -> processIfElse tokens stack

  -- Bitshift operators (Completion)
  "<<" -> processBitshift shiftLeft tokens stack
  ">>" -> processBitshift shiftRight tokens stack

  -- Default: parse as a value and push to stack
  _ -> processStack tokens (parseToken token : stack)

-- Process integer operations
processIntOp :: (Int -> Int -> Int) -> [String] -> [StackElement] -> [StackElement]
processIntOp op tokens ((IntVal b):(IntVal a):rest) = processStack tokens (IntVal (op a b) : rest)
processIntOp _ tokens stack = processStack tokens stack

-- Process string operations
processStrOp :: (String -> String -> String) -> [String] -> [StackElement] -> [StackElement]
processStrOp op tokens ((StrVal b):(StrVal a):rest) = processStack tokens (StrVal (op a b) : rest)
processStrOp _ tokens stack = processStack tokens stack

-- Process stack operations
processDrop :: [String] -> [StackElement] -> [StackElement]
processDrop tokens [] = processStack tokens []
processDrop tokens (_:rest) = processStack tokens rest

processDup :: [String] -> [StackElement] -> [StackElement]
processDup tokens [] = processStack tokens []
processDup tokens (x:xs) = processStack tokens (x:x:xs)

processSwap :: [String] -> [StackElement] -> [StackElement]
processSwap tokens (a:b:rest) = processStack tokens (b:a:rest)
processSwap tokens stack = processStack tokens stack

processRot :: [String] -> [StackElement] -> [StackElement]
processRot tokens (a:b:c:rest) = processStack tokens (c:a:b:rest)
processRot tokens stack = processStack tokens stack

processRoll :: [String] -> [StackElement] -> [StackElement]
processRoll tokens ((IntVal n):stack)
    | n > 0 && length stack >= n =
        let (top, bottom) = splitAt n stack
            rotated = last top : init top
        in processStack tokens (rotated ++ bottom)
    | otherwise = processStack tokens stack
processRoll tokens stack = processStack tokens stack

processRolld :: [String] -> [StackElement] -> [StackElement]
processRolld tokens ((IntVal n):stack)
    | n > 0 && length stack >= n =
        let (top, bottom) = splitAt n stack
            rotated = tail top ++ [head top]
        in processStack tokens (rotated ++ bottom)
    | otherwise = processStack tokens stack
processRolld tokens stack = processStack tokens stack

-- Process comparison operations
processCompOp :: (forall a. Ord a => a -> a -> Bool) -> [String] -> [StackElement] -> [StackElement]
processCompOp op tokens ((IntVal b):(IntVal a):rest) = processStack tokens (BoolVal (op a b) : rest)
processCompOp op tokens ((StrVal b):(StrVal a):rest) = processStack tokens (BoolVal (op a b) : rest)
processCompOp _ tokens stack = processStack tokens stack

-- Process spaceship operator (<=>)
processSpaceship :: [String] -> [StackElement] -> [StackElement]
processSpaceship tokens ((IntVal b):(IntVal a):rest) =
  processStack tokens (IntVal (fromEnum (compare a b)) : rest)
processSpaceship tokens ((StrVal b):(StrVal a):rest) =
  processStack tokens (IntVal (fromEnum (compare a b)) : rest)
processSpaceship tokens stack = processStack tokens stack

-- Process boolean operations
processBoolOp :: (Bool -> Bool -> Bool) -> [String] -> [StackElement] -> [StackElement]
processBoolOp op tokens ((BoolVal b):(BoolVal a):rest) = processStack tokens (BoolVal (op a b) : rest)
processBoolOp _ tokens stack = processStack tokens stack

processBoolXor :: [String] -> [StackElement] -> [StackElement]
processBoolXor tokens ((BoolVal b):(BoolVal a):rest) = processStack tokens (BoolVal (a /= b) : rest)
processBoolXor tokens stack = processStack tokens stack

-- Process bitshift operations
shiftLeft :: Int -> Int -> Int
shiftLeft a b = a * (2^b)

shiftRight :: Int -> Int -> Int
shiftRight a b = a `div` (2^b)

processBitshift :: (Int -> Int -> Int) -> [String] -> [StackElement] -> [StackElement]
processBitshift op tokens ((IntVal b):(IntVal a):rest) = processStack tokens (IntVal (op a b) : rest)
processBitshift _ tokens stack = processStack tokens stack

-- Process IFELSE operation
processIfElse :: [String] -> [StackElement] -> [StackElement]
processIfElse tokens ((BoolVal cond):thenVal:elseVal:rest) =
    processStack tokens ((if cond then thenVal else elseVal) : rest)
processIfElse tokens stack = processStack tokens stack

-- Main processing function
processInput :: String -> [StackElement]
processInput input =
    let tokens = concatMap words (lines input)
    in reverse $ processStack tokens []

-- Function to extract three digits from the input filename
extractDigits :: String -> String
extractDigits filename =
    let digits = filter isDigit filename
    in if length digits >= 3 then take 3 digits else error "Invalid filename format"

-- Main function
main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Usage: ws.hs input-xyz.txt"
        else do
            let inputFile = head args
            let outputFile = "output/output-" ++ extractDigits inputFile ++ ".txt"

            content <- readFile inputFile

            -- Try to process the input and handle any exceptions
            result <- try (evaluate (processInput content)) :: IO (Either SomeException [StackElement])
            case result of
                Left _ -> writeFile outputFile ""  -- Empty output on error
                Right stack -> writeFile outputFile (unlines (map show stack))
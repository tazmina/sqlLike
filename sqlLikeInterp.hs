{-
  Name: <Tazmina Sharmin>
  Class: CS 252
  Assigment: Project
  Date: <April 8, 2018>
  Description: <Describe the program and what it does>
-}


module SqlLikeInterp (
  Expression(..),
  runFile,
  showParsedExp,
  run
) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except

-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String

type Attributes = [Variable]

type Payload = [Attributes]

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Payload

data Expression =
    Add Variable Attributes
  | Put Attributes Variable
  | Get Attributes Variable
  | Del Attributes Variable
  | Sequence Expression Expression
  | Noop
  deriving (Show)

fileP :: GenParser Char st Expression
fileP = do
  prog <- exprP
  eof
  return prog

exprP = do
  e <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing -> e
    Just e' -> Sequence e e')

restSeqP = do
  char ';'
  exprP

-- Expressions are divided into terms and expressions for the sake of
-- parsing.  Note that binary operators **DO NOT** follow the expected
-- presidence rules.
--
-- ***FOR 2pts EXTRA CREDIT (hard, no partial credit)***
-- Correct the precedence of the binary operators.
exprP' = do
  spaces
  t <- termP
  spaces
  return t

-- All terms can be distinguished by looking at the first character
termP = addP
    <|> getP
    <|> putP
    <|> delP
    <|> emptyP
    <?> "add, put, del or get"

emptyP = do
  _ <- spaces
  return $ Noop

addP = do
  _ <- string "add"
  _ <- spaces
  e1 <- varP
  _ <- spaces
  e2 <- attributesP
  return $ Add e1 e2

varP = do
  vStr <- many1 letter
  return vStr

attributesP = do
  _ <- char '('
  v <- varPs
  _ <- char ')'
  return v

varPs = sepBy cell (char ',')

cell = do
  _ <- spaces
  p <- many (noneOf ",\n) ")
  _ <- spaces
  return p

putP = do
  _ <- string "put"
  _ <- spaces
  e <- attributesP
  _ <- spaces
  _ <- string "to"
  _ <- spaces
  e1 <- varP
  return $ Put e e1    

getP = do
  _ <- string "get"
  _ <- spaces
  e <- attributesP
  _ <- spaces
  _ <- string "from"
  _ <- spaces
  e1 <- varP
  return $ Get e e1

delP = do
  _ <- string "del"
  _ <- spaces
  e <- attributesP
  _ <- spaces
  _ <- string "from"
  _ <- spaces
  e1 <- varP
  return $ Del e e1

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

evaluate (Add var attrs) s = do
  case (Map.lookup var s) of
    Nothing -> return ("Added table: " ++ var ++ ", attributes: " ++ stringArray(attrs) ++ "\n", Map.insert var [attrs] s)
    Just v -> return("Table already exists: " ++ var ++ ", attributes: " ++ stringArray(head(v)) ++ "\n", s)

evaluate (Put attrs var) s = do
  case (Map.lookup var s) of
    Nothing -> return ("Table doesn't exist: " ++ var ++ "\n", s)
    Just v -> if (length(attrs) == length(head(v)))
      then return (
        "Added record: " ++ stringArray(attrs) ++ ", to table: " ++ var ++ ", attributes: " ++ stringArray(head(v)) ++ "\n",
        Map.insert var (v ++ [attrs]) s
      )
      else return (
        "Need " ++ show (length (head(v))) ++ " attributes: " ++ stringArray (head(v)) ++ " for table: " ++ var ++ ", but given " ++ show (length (attrs)) ++ " attributes: " ++ stringArray (attrs) ++ "\n",
        s
      )

evaluate (Get attrs var) s = do
  case (Map.lookup var s) of
    Nothing -> return ("Table doesn't exist: " ++ var ++ "\n", s)
    Just v -> if (length(attrs) <= length(head(v)))
      then do
        -- filter and take only first len(attr) elements
        let vMatched = map (take (length(attrs))) ([head(v)] ++ matchRows attrs (tail(v)))
        return ("=== Showing " ++ show (length (tail(vMatched))) ++ " Record(s) from Table: " ++ var ++ "\n" ++ (intercalate "\n" (map (stringArray) vMatched)) ++ "\n===\n", s)
      else return (
        "Need " ++ show (length (head(v))) ++ " or fewer attributes: " ++ stringArray (head(v)) ++ " for table: " ++ var ++ ", but given " ++ show (length (attrs)) ++ " attributes: " ++ stringArray (attrs) ++ "\n",
        s
      )

evaluate (Del attrs var) s = do
  case (Map.lookup var s) of
    Nothing -> return ("Table doesn't exist: " ++ var ++ "\n", s)
    Just v -> if (length(attrs) <= length(head(v)))
      then do
        -- filter and keep both deleted and remaning items
        let vMatched = [head(v)] ++ matchRows attrs (tail(v))
        let vUnmatched = [head(v)] ++ unmatchRows attrs (tail(v))
        return ("=== Deleted " ++ show (length (tail(vMatched))) ++ " Record(s) from Table: " ++ var ++ "\n" ++ (intercalate "\n" (map (stringArray) vMatched)) ++ "\n===\n", Map.insert var vUnmatched s)
      else return (
        "Need " ++ show (length (head(v))) ++ " or fewer attributes: " ++ stringArray (head(v)) ++ " for table: " ++ var ++ ", but given " ++ show (length (attrs)) ++ " attributes: " ++ stringArray (attrs) ++ "\n",
        s
      )

evaluate (Sequence e1 e2) s = do
  (v1, s1) <- evaluate e1 s
  (v2, s') <- evaluate e2 s1
  return (v1 ++ v2, s')

evaluate (Noop) s = do
  return ("", s)

-- Given filter attributes and multiple rows, returns an array with matched rows
matchRows :: Attributes -> [Attributes] -> [Attributes]
matchRows _ [] = []
matchRows filterAttrs rows = if (matchRow filterAttrs (head(rows)))
  then [head(rows)] ++ matchRows filterAttrs (tail(rows))
  else [] ++ matchRows filterAttrs (tail(rows))

-- Given filter attributes and multiple rows, returns an array with non-matched rows
unmatchRows :: Attributes -> [Attributes] -> [Attributes]
unmatchRows _ [] = []
unmatchRows filterAttrs rows = if (matchRow filterAttrs (head(rows)))
  then [] ++ unmatchRows filterAttrs (tail(rows))
  else [head(rows)] ++ unmatchRows filterAttrs (tail(rows))

-- Given filter attributes and a row, returns true if the filter attribtues apply to the row 
matchRow :: Attributes -> Attributes -> Bool
matchRow [] _ = True
matchRow ["_"] _ = True
matchRow filter row = if ((head(filter) == "_") || (head(filter) == head(row)))
  then matchRow (tail(filter)) (tail(row))
  else False

stringArray :: Attributes -> String
stringArray a = "[" ++ (intercalate ", " (a)) ++ "]"

run :: Expression -> Either ErrorMsg (Variable, Store)
run prog = evaluate prog Map.empty

runFile fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> putStr v

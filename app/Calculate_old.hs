-- 6:51pm

module Calculate where 

import Data.Char 
import Control.Monad
import Debug.Trace

-- how do you track NaN error vs parse error? 

cutWhitespace :: String -> String 
cutWhitespace = filter (\c -> c /= ' ')

data Op = Plus | Minus | Times | Divide deriving (Show, Eq)
data Parens = L | R deriving (Show, Eq)
data Term = Number Double | Operation Op | Parentheses Parens deriving (Show, Eq)

appendMaybe :: a -> Maybe [a] -> Maybe [a] 
appendMaybe x zs = case zs of 
  Just ws -> Just $ x:ws 
  Nothing -> Nothing 


-- if there's an expression that starts with a valid int or double, followed by an op, split it appropriately
numSplit :: String -> Maybe (Double, String)
numSplit ('.':cs) = numSplit ("0."++cs) -- Haskell's read doesn't do well with 0.
numSplit cs = case numSplitDecimalCount cs 0 of 
  Just ("", y) -> Nothing 
  Just (".", y) -> Nothing 
  Just (x, y) -> Just (read x, y)
  Nothing -> Nothing

numSplitDecimalCount :: String -> Int -> Maybe (String, String)
numSplitDecimalCount "" _ = Just ("", "")
numSplitDecimalCount ('.':cs) 0 = case numSplitDecimalCount cs 1 of 
  Just (x,y) -> Just ("." ++ x, y)
  Nothing -> Nothing 
numSplitDecimalCount ('.':cs) _ = Nothing -- means there are too many decimal points
numSplitDecimalCount (c:cs) d = case isDigit c of 
  True -> case numSplitDecimalCount cs d of 
    Just (x,y) -> Just (c:x, y)
    Nothing -> Nothing 
  False -> Just ("", c:cs)

parseTerms :: String -> Maybe [Term]
parseTerms "" = Just []
parseTerms (c:cs) = case c of 
  '+' -> appendMaybe (Operation Plus) (parseTerms cs)
  '-' -> appendMaybe (Operation Minus) (parseTerms cs)
  '*' -> appendMaybe (Operation Times) (parseTerms cs)
  '/' -> appendMaybe (Operation Divide) (parseTerms cs)
  '(' -> appendMaybe (Parentheses L) (parseTerms cs)
  ')' -> appendMaybe (Parentheses R) (parseTerms cs)
  _   -> case numSplit (c:cs) of 
    Nothing -> Nothing 
    Just (n, ts) -> appendMaybe (Number n) (parseTerms ts)

-- terms to left outside first left parens, terms inside parens, terms right outside corresponding closing parens
parensSplit :: [Term] -> Maybe ([Term], [Term], [Term]) 
parensSplit [] = Just ([], [], [])
parensSplit ((Parentheses R):_) = Nothing 
parensSplit ((Parentheses L):ts) = Just ([], b, c)
  where 
    (b,c) = splitByClosingParens ts 1 
    splitByClosingParens :: [Term] -> Int -> ([Term], [Term])
    splitByClosingParens ((Parentheses L):xs) counter = ((Parentheses L) : x, y)
      where (x,y) = splitByClosingParens xs (counter+1)
    splitByClosingParens ((Parentheses R):xs) 1 = ([], xs)
    splitByClosingParens ((Parentheses R):xs) counter = ((Parentheses R) : x, y)
      where (x,y) = splitByClosingParens xs (counter-1)
    splitByClosingParens (x:xs) counter = (x:w, z)
      where (w, z) = splitByClosingParens xs (counter)
parensSplit (t:ts) = case parensSplit ts of 
  Just (a,b,c) -> Just $ (t:a, b, c)
  Nothing -> Nothing 

lookUp :: Op -> Double -> Double -> Double
lookUp Plus = (+)
lookUp Minus = (-)
lookUp Times = (*)
lookUp Divide = (/)

-- greedily searches for the Op, and replaces (left, op, right) with application of op on left and right
-- assumes no parens (#TODO rearrange type signatures such that this error is impossible)
evalOps :: Op -> [Term] -> Maybe [Term] 
evalOps op ts = case ts of 
  [] -> Just [] 
  [Number n] -> Just [Number n]
  (Number n1):op':remainder -> case (op' == Operation op) of 
    True -> case remainder of 
      (Number n2):rest -> evalOps op $ (Number $ (lookUp op) n1 n2):rest 
      --
    False -> appendMaybe (Number n1) $ appendMaybe op' $ evalOps op remainder 
  -- x -> trace ("line 104" ++ (show $ Just x)) (Just x)

collapseOps :: [Term] -> Maybe [Term]  
collapseOps = (evalOps Plus) <=< (evalOps Minus) <=< (evalOps Times) <=< (evalOps Divide)

nakedEval :: [Term] -> Maybe Double 
nakedEval s = case collapseOps s of 
  Nothing -> Nothing 
  Just [Number x] ->  Just x
  _ -> Nothing 

parensEval :: [Term] -> Maybe Double 
parensEval ts = case parensSplit ts of 
  Nothing -> Nothing 
  Just (a, [], b) -> nakedEval a 
  Just (a, b, c) -> case parensEval b of 
    Nothing -> Nothing 
    Just x -> parensEval $ a ++ [Number x] ++ c

calculate :: String -> Maybe Double 
calculate = (parensEval <=< parseTerms) . cutWhitespace

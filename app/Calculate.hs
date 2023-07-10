module Calculate where 

import Text.Read (readMaybe)
import Data.List.Split
import Control.Monad

-- how this works: 
-- (1) parses string
--   (1a) find all opening and closing parentheses, and represent in data structure that tracks the nesting
--   (1b) for each parenthesis-free ("naked") string, split into a sequence of doubles and operators
-- (2) recursively evals the data structure by substituting in doubles for each parenthetical expression
--   (2a) for each "naked" expression, iteratively "reduce" it by first evaling all the exponents, 
--        then all the divisions, then multiplications, etc. 

data Operation = Plus | Minus | Times | Divide | Exp deriving (Show, Eq)
data Term = Number Double | Op Operation deriving (Show, Eq) 
data Expr = NakedExpr [Term] | ParensExpr [Term] Expr Expr deriving (Show, Eq)
-- everything with matching parentheses can be split into 
--   everything before first parenthesis
--   everything non-inclusively between first opening parens and corresponding closing parens
--   everything after that closing parens
-- 1+(2+3)+5 would get parsed into ParensExpr [1,+] (NakedExpr [2,+,3]) (NakedExpr [+,5])

lookUp :: Operation -> Double -> Double -> Double
lookUp Plus = (+)
lookUp Minus = (-)
lookUp Times = (*)
lookUp Divide = (/)
lookUp Exp = (**)

data ParseError = ParensMismatch | EmptyInput | InvalidNumber | OperatorError | AssertFalse String deriving (Show, Eq)

-- parsing
-------------------------------------------------------------------------------

isOperator :: Char -> Bool 
isOperator = (flip elem) ['+', '-', '*', '/', '^']

-- turns a numerical string into a (Number Double), and an operation into 
-- an Op Operation 
parseTerm :: String -> Either ParseError Term 
parseTerm "" = Left $ AssertFalse "parseTerm should never be fed an empty input"
parseTerm s = case isOperator $ head s of 
  False -> case readMaybe s of 
    Just d -> Right $ Number d 
    Nothing -> Left InvalidNumber
  True -> case head s of 
    '+' -> Right (Op Plus)
    '-' -> Right (Op Minus)
    '*' -> Right (Op Times)
    '/' -> Right (Op Divide)
    '^' -> Right (Op Exp)
    _ -> Left $ AssertFalse "forbidden case of parseTerm"

-- examples
-- 2+3.5-5+ --> ["2", "+", "3.5", "-", "5","+"]
-- -5+35v-5 --> ["-", "5", "35v", "-", "5"]
splitByOperator :: String -> [String] 
splitByOperator = filter (not . null) . split (whenElt isOperator)

-- like ghci, we don't process negative numbers, unless they are right at
-- the beginning of an expression to compute, e.g. the original string 
-- (e.g. like in "-5+3"), or the contents within parentheses (e.g. like the 
-- "-1-2" in "5*(-1-2)"), as opposed to somewhere in the weeds of an 
-- expression we're parsing (for example, the "-3" in "(1+2)-3"); this is
-- what the first input tracks. since this function is only called in
-- parenthesis-free contexts, we can use a simple hack to replace minus 
-- signs at the start of a string with 0-. Example:
-- -5+3 ==> ["-","5","+","3"] ==> ["0","-","5","+","3"]
negativeMod :: Bool -> [String] -> [String]
negativeMod True ("-":s) = "0":"-":s
negativeMod _ s = s

-- turns a parenthesis-free expression into a list of operators and numerical terms. 
parseNaked :: Bool -> String -> Either ParseError [Term] 
parseNaked isHead = sequence . (fmap parseTerm) . (negativeMod isHead) . splitByOperator

-- ex: "1+(2+5)-10" ==> ("1+", "2+5", "-10")
parensSplit :: String -> Either ParseError (String, String, String) 
parensSplit [] = Right ([], [], [])
parensSplit (')':_) = Left ParensMismatch
parensSplit ('(':ts) = fmap (\(w,z) -> ([], w, z)) $ splitByClosingParens ts 1 
  where 
    splitByClosingParens :: String -> Int -> Either ParseError (String, String)
    -- pop another '(' onto the stack
    splitByClosingParens ('(':xs) c = (\(x,y) -> ('(':x, y)) <$!> splitByClosingParens xs (c+1)
    -- pop ')' off the stack and failing
    splitByClosingParens (')':_) 0 = Left ParensMismatch
    -- pop ')' off the stack, now we know we've matched the original
    splitByClosingParens (')':xs) 1 = Right ([], xs)
    -- pop ')' off the stack, but there's still more to pop off
    splitByClosingParens (')':xs) c = (\(x,y) -> (')':x, y)) <$!> splitByClosingParens xs (c-1)
    -- for if we see any old character
    splitByClosingParens (x:xs) c = (\(w,z) -> (x:w, z)) <$!> splitByClosingParens xs c
    -- ^ just appends the first char of your string to the left tuple of what 
    -- you get when you recursively call the func on the rest of your string 
    splitByClosingParens [] _ = Left ParensMismatch
parensSplit (t:ts) = (\(a,b,c) -> (t:a, b, c)) <$!> parensSplit ts
-- ^ just appends the first char of your string to the leftmost tuple of what 
-- you get when you recursively call the func on the rest of your string 

-- We need to track whether the string we're parsing is a "head" (at the very
-- beginning of an expression we're evaling, or the inside of parentheses) or 
-- a "tail" (content after a closing parentheses) in order to correctly discern 
-- whether a "-" makes a number negative, or whether it's a minus sign after a
-- closing parenthesis. See comment for negativeMod for examples. 
parseExpr' :: Bool -> String -> Either ParseError Expr 
parseExpr' isHead s = case parensSplit s of 
  -- no parens
  Right (_, "", "") -> NakedExpr <$!> parseNaked isHead s 
  -- yes parens
  Right (a, b, c) -> case (parseNaked isHead a, parseExpr' True b, parseExpr' False c) of 
    (Right x, Right y, Right z) -> Right $ ParensExpr x y z 
    (Left a, _, _) -> Left a 
    (_, Left b, _) -> Left b 
    (_, _, Left c) -> Left c
  Left e -> Left e

parseExpr :: String -> Either ParseError Expr 
parseExpr = parseExpr' True 

-- evaluations
-------------------------------------------------------------------------------

-- greedily (left-to-right) searches for the specified operator, and
-- replaces each instace of [left, op, right] with an application of 
-- op on the terms, e.g. replaces [5, -, 2] --> [3]
-- ex: evalOp [2,-,4,+,7,-,9] Minus = evalOp [-2,+,7,-,9] Minus = [-2,+,-2]
evalOp :: Operation -> [Term] -> Either ParseError [Term] 
evalOp op ts = case ts of 
  [] -> Right [] 
  [Number n] -> Right [Number n] 
  (Number n1):op':(Number n2):rest -> case (op' == Op op) of 
    -- ex: pretend the op = Plus
    -- ex of True case: input is [12,+,5,-,7,+,8], output is evalOp Plus [17,-,7,+,8]
    True -> evalOp op $ (Number $ (lookUp op) n1 n2):rest 
    -- ex of False case: input is [12,/,5,-,7,+,8], output is [12,/] ++ evalOp Plus [5,-,7,+,8]
    False -> ([Number n1, op'] ++) <$!> evalOp op ((Number n2):rest)
  _ -> Left $ OperatorError

-- runs evalOp on all operations; the list is ordered in reverse order of operations
collapseOps :: [Term] -> Either ParseError [Term]  
collapseOps = foldr (<=<) return $ fmap evalOp [Plus, Minus, Times, Divide, Exp]
-- ^ fancy Haskell way of compressing the below line of code
-- (evalOp Plus) <=< (evalOp Minus) <=< (evalOp Times) <=< (evalOp Divide) <=< (evalOp Exp)
-- ^ fancy Haskell way of saying "evalPlus . evalMinus . evalTimes . evalDivide . evalExp", but
-- which automatically handles the error checking in a typesafe way

nakedEval :: [Term] -> Either ParseError Double 
nakedEval s = case collapseOps s of 
  Right [Number x] ->  Right x
  Right [] -> Left EmptyInput
  Right _ -> Left $ AssertFalse "forbidden case of nakedEval"
  Left e -> Left e 

parensEval :: Expr -> Either ParseError Double 
parensEval s = case s of 
  NakedExpr s -> nakedEval s -- 8/4 ==> 2
  ParensExpr a b c -> case (parensEval b, c) of -- 1+(24/4)+whatever 
    (Right x, NakedExpr ts) -> nakedEval $ a ++ [Number x] ++ ts 
    -- ^ ex: 1+(24/4)+9 ==> nakedEval 1+6+9
    (Right x, ParensExpr ts y z) -> parensEval $ ParensExpr (a ++ [Number x] ++ ts) y z 
    -- ^ ex: 1+(24/4)+5-7-(8+9)+10 --> parensEval $ ParensExpr (1+6+5-7-) (8+9) (+10)
    (Left e, _) -> Left e 

calculate :: String -> Either ParseError Double 
calculate = (parensEval <=< parseExpr) . cutWhitespace
  where cutWhitespace = filter (\c -> c /= ' ')

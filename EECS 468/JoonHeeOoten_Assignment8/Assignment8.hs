{--
Name: Joon Hee Ooten
Last modified: 11/27/23
--}
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Monad (void)
import Control.Applicative ((<|>))

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Pow Expr Expr
          | Const Double
          deriving (Show, Read)

parseExpr :: String -> Maybe Expr
parseExpr s = case parseExpr' s of
  [(expr, "")] -> Just expr
  _ -> Nothing

parseExpr' :: String -> [(Expr, String)]
parseExpr' = readP_to_S exprParser

exprParser :: ReadP Expr
exprParser = chainl1 term addSubOp

addSubOp :: ReadP (Expr -> Expr -> Expr)
addSubOp = (Add <$ char '+') +++ (Sub <$ char '-')

term :: ReadP Expr
term = chainl1 factor mulDivOp

mulDivOp :: ReadP (Expr -> Expr -> Expr)
mulDivOp = (Mul <$ char '*') +++ (Div <$ char '/') +++ (Mod <$ char '%')

factor :: ReadP Expr
factor = parens exprParser +++ constParser +++ power

power :: ReadP Expr
power = do
  char '('
  base <- exprParser
  string "**"
  exp <- exprParser
  char ')'
  return (Pow base exp)

constParser :: ReadP Expr
constParser = Const . read <$> munch1 isDigit

number :: ReadP Expr
number = Const . read <$> many1 (satisfy (\c -> isDigit c || c == '.'))

parens :: ReadP a -> ReadP a
parens p = char '(' *> p <* char ')'

eval :: Expr -> Either String Double
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  if v2 /= 0 then
    pure (v1 / v2)
  else
    Left "Division by zero"
eval (Mod e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  if v2 /= 0 then
    pure (mod' v1 v2)
  else
    Left "Modulo by zero"
eval (Pow e1 e2) = (**) <$> eval e1 <*> eval e2
eval (Const x)   = pure x

mod' :: Double -> Double -> Double
mod' x y = x - y * fromIntegral (floor (x / y))

runParser :: ReadP a -> String -> [(a, String)]
runParser p input = case readP_to_S p input of
  [] -> []
  result : _ -> [result]

main :: IO ()
main = do
  putStrLn "Enter an arithmetic expression surrounded with parenthesis (or type 'quit' to exit):"
  input <- getLine
  if input == "quit"
    then putStrLn "Exiting program."
    else case parseExpr input of
           Just ast -> case eval ast of
             Left err -> putStrLn $ "Error: " ++ err
             Right result -> putStrLn $ "Result: " ++ show result
           Nothing -> putStrLn "Error: Invalid expression"
         >> main
         
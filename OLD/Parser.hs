import Data.Char (ord, isDigit)

import Text.Parsec hiding (digit, token)
import Text.Parsec.Combinator (chainl1)
import Text.Parsec.String

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit <|> do {symb "("; n <- expr; symb ")"; return n}

digit :: Parser Int
digit = do {x <- token (satisfy isDigit); return (ord x - ord '0')}

addop :: Parser (Int -> Int -> Int)
addop = do {symb "+"; return (+)} <|> do {symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} <|> do {symb "/"; return (div)}

token :: Parser a -> Parser a
token p = p <* spaces

symb :: String -> Parser String
symb w = token (string w)




-- Define the abstract syntax tree data type
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Lit Int
          deriving Show

-- Expression parser returning an abstract syntax tree
expr' :: Parser Expr
expr' = term' `chainl1` addop'

-- Term parser returning an abstract syntax tree
term' :: Parser Expr
term' = factor' `chainl1` mulop'

-- Factor parser returning an abstract syntax tree
factor' :: Parser Expr
factor' = digit' <|> do {symb' "("; n <- expr'; symb' ")"; return n}

-- Digit parser returning an abstract syntax tree
digit' :: Parser Expr
digit' = do {x <- token' (satisfy isDigit); return (Lit (ord x - ord '0'))}

-- Add operator parser returning an operation node
addop' :: Parser (Expr -> Expr -> Expr)
addop' = do {symb' "+"; return Add} <|> do {symb' "-"; return Sub}

-- Mul operator parser returning an operation node
mulop' :: Parser (Expr -> Expr -> Expr)
mulop' = do {symb' "*"; return Mul} <|> do {symb' "/"; return Div}

-- Token parser
token' :: Parser a -> Parser a
token' p = p <* spaces

-- Symbol parser
symb' :: String -> Parser String
symb' w = token' (string w)

-- * HOMEWORK 5B
--
-- 1) Modify the above parser to return an abstract syntax tree
--    instead of an integer.
--
-- 2) Augment the parser to parse a statement.
--
--      expression e ::= 0 | ... | 9 
--                    | e1 + e2 | e1 * e2 | e1 - e2 | e1 / e2
--
--      statement s ::= s1 ; s2
--                    | x = e

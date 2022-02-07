import Data.List
import Text.ParserCombinators.Parsec

-- Declara termos
data Term = Atom String
          | Variable Name
          | Predicate (String, [Term])
            deriving Show
--
-- Atom :: String -> Term
-- Variable :: Name -> Term
-- Predicate :: (String, [Term]) -> Term
--

-- Declara nomes
type Name = String

-- Declara unificadores
type Unifier = [(Name, Term)]

--
-- Lê um átomo
--   atom: lowercase+
--
atom :: Parser Term
atom = do
  -- lower :: Parser Char
  -- many1 :: Parser a -> Parser [a]
  -- many1 lower :: Parser String
  name <- many1 lower
  -- name :: String
  return $ Atom name


main :: IO ()
main = do
    putStrLn "Digite um termo:"
    str <- getLine
    print $ parse atom "<stdin>" str
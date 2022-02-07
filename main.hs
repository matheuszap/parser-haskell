import Data.List
import Text.ParserCombinators.Parsec

-- Declara termos
data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
        deriving Show

-- Declara nomes
type Name = String

-- Declara unificadores
type Unifier = [(Name, Type)]

--
-- { X |-> Y }
--

test :: Unifier
test = [
        ("X", TypeVar "Y")
       ]

main :: IO ()
main = do
    putStrLn "Hello, world!"
    print test
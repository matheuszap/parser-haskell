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

-- Árvore de derivações: 
--
--                     parseType
--                    /         \
--               parseFun       parseAtom
--                /   \             /  |  \
--        parseAtom  parseType  pInt  pVar pParen
--       /   |  \      /   \        
--    ...  ...  ...  ...  ...

whitespace :: Parser ()
whitespace = do
        many (char ' ')
        return ()

-- int: "Int"
parseInt :: Parser Type
parseInt = do
        char 'I'
        char 'n'
        char 't'
        return TypeInt

-- var: lowercase+
parseVar :: Parser Type
parseVar = do
        variable <- many1 lower
        return (TypeVar variable)

-- paren: "(" type ")"
parseParen :: Parser Type 
parseParen = do
        char '('
        atom <- parseAtom
        char ')'
        return atom

-- atom: int | var | paren
parseAtom :: Parser Type
parseAtom = try
        parseInt <|> parseVar <|> parseParen

-- fun: atom "->" type
parseFun :: Parser Type
parseFun = do
        atom <- parseAtom
        whitespace
        char '-'
        char '>'
        whitespace
        TypeArrow atom <$> parseType

-- type: function | atom
parseType :: Parser Type
parseType = try
        parseFun <|> parseAtom

-- unit: type eof
unit :: Parser Type
unit = do
        t <- parseType
        eof
        return t

occursCheck :: Name -> Type -> Bool
occursCheck x TypeInt = 
        False
occursCheck x (TypeVar y) =
         x == y
occursCheck x (TypeArrow y z) =  
        occursCheck x y || occursCheck x z

--compose :: Unifer -> Unifier -> Unifier
--compose xs ys = xs ++ ys

-- unify :: Type -> Type -> Maybe Unifier

main :: IO ()
main = do
    putStrLn "Digite um termo:"
    str <- getLine
    print $ parse unit "<stdin>" str




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

-- occursCheck "x" TypeInt
-- 1st Case: False
--
-- occursCheck "x" (TypeVar "x")
-- 2nd Case: True (x == y)
--
-- occursCheck "x" (TypeArrow (TypeVar "x") TypeInt)
-- 3rd Case: occursCheck "x" (TypeVar "x") || occursCheck "x" TypeInt
--                         True (2nd Case) || False (1st Case)
--                                       True

-- type Unifier = [(Name, Type)]

-- compose: composição de duas substituições, retornando a união dos dados
-- tento a primeira substituição aplicada a todos os termos da segunda substituição
compose :: Unifier -> Unifier -> Unifier
compose xs ys = xs ++ fmap substElement ys
                where 
                substElement :: (Name, Type) -> (Name, Type)
                substElement (x, e) = (x, subst xs e)

--  compose [("b", TypeInt)] [("a", TypeInt)] --> (b -> Int) * (a -> Int)
--  [("b", TypeInt)] ++ fmap substElement [("a", TypeInt)]
--  [("b", TypeInt)] ++ fmap [("a", subst [("b", TypeInt)] TypeInt)]
--  [("b", TypeInt)] ++ [("a", TypeInt)]
--  [("b", TypeInt), ("a", TypeInt)]
--      {b |-> Int, a |-> Int}


-- subst: aplica a substituição a um tipo arbitrário, retornando um novo tipo
subst :: Unifier -> Type -> Type
subst uni TypeInt = TypeInt
subst uni (TypeVar x) = case lookup x uni of
                        Just n -> n
                        Nothing -> TypeVar x
subst uni (TypeArrow y z) = TypeArrow (subst uni y) (subst uni z)

-- subst [("x", TypeInt)] TypeInt --> {x |-> Int}(Int)
-- 1st Case: TypeInt
--
-- subst [("x", TypeInt)] (TypeVar "x") --> {x |-> Int}(x)
-- 2nd Case: lookup "x" [("x", TypeInt)] == Just TypeInt
--                            TypeInt
--
-- subst [("x", TypeInt)] (TypeArrow (TypeVar "x") (TypeVar "y")) --> {x |-> Int}(x -> y)
-- 3rd Case: TypeArrow (subst [("x", TypeInt)] (TypeArrow (TypeVar "x")) (subst [("x", TypeInt)] (TypeVar "y"))
--                                             TypeArrow TypeInt (TypeVar "y") 
--                                                      Int -> y

unify :: Type -> Type -> Maybe Unifier
unify TypeInt TypeInt = Just []
unify (TypeArrow x y) TypeInt = Nothing
unify TypeInt (TypeArrow x y) = Nothing
unify (TypeVar x) (TypeVar y) | x == y = Just []
unify (TypeVar v) t = if occursCheck v t then Nothing else Just [(v, t)]
unify t (TypeVar v) = if occursCheck v t then Nothing else Just [(v, t)] 
unify (TypeArrow t1 r1) (TypeArrow t2 r2) = do
        s1 <- unify t1 t2
        s2 <- unify (subst s1 r1) (subst s1 r2)
        return (compose s2 s1)

-- unify (TypeArrow (TypeVar "a") TypeInt) (TypeArrow TypeInt (TypeVar "b")) --> a -> Int ~ Int -> b
-- s1 <- unify (TypeVar "a") TypeInt = Just [("a", TypeInt)]
-- s2 <- unify (subst [("a", TypeInt)] TypeInt) (subst [("a", TypeInt)] (TypeVar "b")) = 
--     = unify TypeInt (TypeVar "b")
--     = Just [("b", TypeInt)]
-- return (compose [("b", TypeInt)] [("a", TypeInt)])
--      [("b", TypeInt), ("a", TypeInt)]
--     = {b |-> Int, a |-> Int}
--
--
-- Regras:
--
--    ------------------ (REFL)
--        a ~ a = {}
--
--
--       a não aparece livre em t
--    ---------------------------- (LEFT)
--        a ~ t = { a |-> t }
--       ^^^^^
--
--
--      a não aparece livre em t
--  ---------------------------- (RIGHT)
--      t ~ a = { a |-> t }
--      ^^^^^
--
--
--    ---------------------- (INT)
--        int ~ int = {}
--
--
--        t1 ~ t2 = s1        s1(r1) ~ s1(r2) = s2
--    ------------------------------------------------- (ARROW)
--        t1 -> r1    ~   t2 -> r2    =    s2 * s1
--


main :: IO ()
main = do

  putStrLn "Digite o primeiro termo:"
  str1 <- getLine
  putStrLn "Digite o segundo termo:"
  str2 <- getLine

  case (parse unit "primeiro" str1, parse unit "segundo" str2) of
    (Right e1, Right e2) -> do

      putStrLn "Unificação:"
      print $ unify e1 e2
    (Left err, _) -> do
      print err
    (_, Left err) -> do
      print err



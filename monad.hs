import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: (Parser a) -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item = P (\str -> case str of
          [] -> []
          (x:xs) -> [(x, xs)])

instance Functor Parser where
  -- fmap :: Parser a -> (a -> b) -> Parser b
  fmap f p = P (\str -> case parse p str of
                [] -> []
                [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = P (\str -> [(a, str)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\str -> case parse pf str of
              [] -> []
              [(v, out)] -> parse (fmap v px) out)

instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\str -> case parse p str of
              [] -> []
              [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser
  p <|> q = P (\inp -> case parse p inp of
                [] -> parse q inp
                [(v, out)] -> [(v, out)])

data Operator = Left | Right | Inc | Dec | Input | Output | Begin | End | Noop
    deriving (Show, Eq)
type Block = [Operator]


toOperator :: Char -> Operator
toOperator c = case c of
  '>' -> Main.Left
  '<' -> Main.Right
  '+' -> Inc
  '-' -> Dec
  '.' -> Input
  ',' -> Output
  '[' -> Begin
  ']' -> End
  _   -> Noop

operator :: Parser Operator
operator = do c <- item
              let op = toOperator c
              if op == Noop
                then operator
                else return op

satisfies :: (Operator -> Bool) -> Parser Operator
satisfies p = do op <- operator
                 if p op
                   then return op
                   else empty

is :: Operator -> Parser Operator
is op = satisfies (==op)

begin :: Parser Operator
begin = is Begin

end :: Parser Operator
end = is End

nonControlOperator :: Parser Operator
nonControlOperator = satisfies (\op -> elem op [Main.Left,Main.Right,Inc,Dec,Input,Output,Noop])

block :: Parser Block
block = do b      <- begin
           ops    <- many nonControlOperator
           blocks'<- many block
           ops'   <- many nonControlOperator
           e      <- end
           return ([b] ++ ops ++ (concat blocks') ++ ops' ++ [e])

code :: Parser Block
code = do ops     <- many nonControlOperator
          blocks' <- many block
          ops'    <- many nonControlOperator
          return (ops ++ (concat blocks') ++ ops')

program :: Parser Block
program = do c1 <- code
             c2 <- code
             return (c1 ++ c2)
module Main (main) where

import Data.Int (Int)
import Control.Monad.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data NodeTypes =
      CONST
    | VAR
    | ADD
    | SUB
    | MUL
    | DIV
    | LESSTHEN
    | SET
    | IF
    | IFELSE
    | WHILE
    | SEQ
    | EXPR
    | PROG
    | EMPTY
    | PRINT
    deriving (Show, Eq)

data TokenTypes =
      Num
    | Var
    | If
    | Else
    | Lbra
    | Rbra
    | Lpar
    | Rpar
    | Add
    | Sub
    | Mul
    | Div
    | Less
    | Assig
    | Endfile
    | Semicol
    | Error
    | Print
    deriving (Show, Eq)

-- 3 - node -> max case of 3 ops (IFELSE)
data Node = Node {
    nodeType :: NodeTypes,
    value :: Maybe Int,
    op1 :: Maybe Node,
    op2 :: Maybe Node,
    op3 :: Maybe Node
} deriving (Show)

data Token = Token {
    tokenType :: TokenTypes,
    tokenValue :: Maybe Int,
    tokenVariable :: Maybe Char
} deriving (Show, Eq)

data CompilerState = CompilerState {
    text :: String,
    textLen :: Int,
    nowPos :: Int,
    tokens :: [Token]
} deriving (Show)

type CompilerM = State -> CompilerState

initCompilerState :: String -> CompilerState
initCompilerState text = text (length text) 0 []

tokenize :: CompilerM -> Bool
tokenize = do
    token <- getNextToken
    case token of
        Token Endfile _ _ -> return true
        Token Error _ _ -> return false
        _ -> do
        modify (\s -> s { tokens = tokens s ++ [token] })
        tokenize

nextChar :: Int -> String -> (Char, Int)
nextChar pos str
    | pos < length str = (str !! pos, pos + 1)
    | otherwise = ('\0', pos) -- end line

getNextToken :: CompilerM Token
getNextToken = do
  state <- get
  let pos = nowPos state
      text' = text state
      textLen' = textLen state
  if pos >= textLen' then return $ Token Endfile Nothing Nothing
  else do
    let (char, newPos) = nextChar pos text'
    if isSpace char  -- skip spases recursively
      then do
        put $ state { nowPos = newPos }
        getNextToken
      else do
        let (token, finalPos) = parseToken newPos char text'  -- Парсим токен
        put $ state { nowPos = finalPos, tokens = tokens state ++ [token] }
        return token

-- Функция для проверки на пробельный символ
isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\r' || c == '\t' || c == '\n'

-- Функция для парсинга токена
parseToken :: Int -> Char -> String -> (Token, Int)
parseToken pos char text'
  | isDigit char = parseNumber pos char text'   -- Парсим число
  | isLetter char = parseVariable pos char text'  -- Парсим переменную
  | isSpecialSymbol char = parseSymbol pos char text' -- Парсим спецсимвол
  | otherwise = (Token Error Nothing Nothing, pos + 1) -- Ошибка: неизвестный символ

-- Парсинг числа
parseNumber :: Int -> Char -> String -> (Token, Int)
parseNumber pos char text' = do
  let (numStr, finalPos) = span isDigit (drop pos text')
      value = read numStr :: Int
  (Token Num (Just value) Nothing, finalPos + pos)


-- Парсинг переменной
parseVariable :: Int -> Char -> String -> (Token, Int)
parseVariable pos char text' = (Token Var Nothing (Just char) , pos + 1)

-- Парсинг спецсимвола (замените на свою логику)
parseSymbol :: Int -> Char -> String -> (Token, Int)
parseSymbol pos char text'
 | char == '+' = (Token Add Nothing Nothing, pos + 1)
 | char == '-' = (Token Sub Nothing Nothing, pos + 1)
 | char == '*' = (Token Mul Nothing Nothing, pos + 1)
 | char == '/' = (Token Div Nothing Nothing, pos + 1)
 | char == '<' = (Token Less Nothing Nothing, pos + 1)
 | char == '=' = (Token Assig Nothing Nothing, pos + 1)
 | char == '(' = (Token Lpar Nothing Nothing, pos + 1)
 | char == ')' = (Token Rpar Nothing Nothing, pos + 1)
 | char == '{' = (Token Lbra Nothing Nothing, pos + 1)
 | char == '}' = (Token Rbra Nothing Nothing, pos + 1)
 | char == ';' = (Token Semicol Nothing Nothing, pos + 1)
 | otherwise = (Token Error Nothing Nothing, pos + 1)


isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isLetter :: Char -> Bool
isLetter c = isAsciiLower c || isAsciiUpper c


isSpecialSymbol :: Char -> Bool -- Здесь перечислены все спецсимволы вашего языка
isSpecialSymbol c = c `elem` "+-*/<(){}="


main :: IO ()
main = do
    let input = "{ a = 6; }"
    initialState = initCompilerState input
    result = evalState

module Main (main) where

import Control.Monad.State
import Data.Char (isAsciiLower, isAsciiUpper)

data NodeTypes =
      CONST | VAR | ADD | SUB | MUL | DIV
    | LESSTHEN | SET | IF | IFELSE | WHILE
    | SEQ | EXPR | PROG | EMPTY | PRINT
    deriving (Show, Eq)

data TokenTypes =
      Num | Var | If | Else | While | Lbra | Rbra
    | Lpar | Rpar | Add | Sub | Mul | Div | Less
    | Assig | Endfile | Semicol | Error | Print
    deriving (Show, Eq)

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

type CompilerM = State CompilerState

initCompilerState :: String -> CompilerState
initCompilerState inputText = CompilerState inputText (length inputText) 0 []

tokenize :: CompilerM [Token]
tokenize = do
    state <- get
    let pos = nowPos state
        text' = text state
        textLen' = textLen state
    if pos >= textLen'
        then return (tokens state)
        else do
            token <- getNextToken
            state' <- get
            put $ state' { tokens = tokens state' ++ [token] }
            tokenize

getNextToken :: CompilerM Token
getNextToken = do
    state <- get
    let pos = nowPos state
        text' = text state
    if pos >= textLen state
        then return $ Token Endfile Nothing Nothing
        else do
            let char = text' !! pos
            if isSpace char
                then do
                    put $ state { nowPos = pos + 1 }
                    getNextToken
                else do
                    let (token, finalPos) = parseToken pos char text'
                    put $ state { nowPos = finalPos }
                    return token

parseToken :: Int -> Char -> String -> (Token, Int)
parseToken pos char text'
    | isDigit char = parseNumber pos text'
    | isLetter char = parseIdentifier pos text'
    | isSpecialSymbol char = parseSymbol pos char
    | otherwise = (Token Error Nothing Nothing, pos + 1)

parseNumber :: Int -> String -> (Token, Int)
parseNumber pos text' =
    let (numStr, rest) = span isDigit (drop pos text')
        value = read numStr
    in (Token Num (Just value) Nothing, pos + length numStr)

parseIdentifier :: Int -> String -> (Token, Int)
parseIdentifier pos text' =
    let (ident, rest) = span isIdentifierChar (drop pos text')
        tokenType' = case ident of
                       "if" -> If
                       "else" -> Else
                       "print" -> Print
                       "while" -> While
                       _ -> Var
    in (Token tokenType' Nothing (if tokenType' == Var then Just (head ident) else Nothing), pos + length ident)

parseSymbol :: Int -> Char -> (Token, Int)
parseSymbol pos char
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

isSpace :: Char -> Bool
isSpace c = c `elem` " \r\n\t"

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isLetter :: Char -> Bool
isLetter c = isAsciiLower c || isAsciiUpper c

isSpecialSymbol :: Char -> Bool
isSpecialSymbol c = c `elem` "+-*/<(){}=;"

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isLetter c || isDigit c || c == '_'

main :: IO ()
main = do
    let input = "{ a = 6; print 5; while (a < 10) { a = a + 10 } }"
        initialState = initCompilerState input
    let (tokens, _) = runState tokenize initialState
    print tokens
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Main (main) where

import Control.Monad.State
import Data.Char (isAsciiLower, isAsciiUpper)

data NodeTypes =
      CONST | VAR | ADD | SUB | MUL | DIV
    | LESSTHEN | SET | IF | IFELSE | WHILE
    | SEQ | EXPR | PROG | EMPTY | PRINT
    deriving (Show, Eq)

data Node = Node {
    nodeType :: NodeTypes,
    nodeValue :: Maybe Int,
    nodeOp1 :: Maybe Node,
    nodeOp2 :: Maybe Node,
    nodeOp3 :: Maybe Node
} deriving (Show, Eq)

emptyNode :: Node
emptyNode = Node EMPTY Nothing Nothing Nothing Nothing

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
type ParserM = State (Int, [Token])

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


---------- build ast

runParser :: [Token] -> Maybe Node
runParser tokens = case runState parse (0, tokens) of
                      (Just node, _) -> Just node
                      (Nothing, _) -> Nothing
parse :: ParserM (Maybe Node)
parse = do
    (idx, tokens) <- get
    if idx >= length tokens
        then return Nothing
        else do
            node <- statement
            (idx', _) <- get
            if idx' == length tokens then return node else return Nothing

statement :: ParserM (Maybe Node)
statement = do
    (idx, tokens) <- get
    if idx >= length tokens
        then return Nothing
        else case tokens !! idx of
            Token If _ _ -> do
                put (idx + 1, tokens)
                op1 <- parenExpr
                op2 <- statement
                (idx', tokens') <- get
                case (op1, op2) of
                    (Just op1', Just op2') -> do
                        if idx' < length tokens' && tokenType (tokens' !! idx') == Else
                            then do
                                put (idx' + 1, tokens')
                                op3 <- statement
                                case op3 of
                                    Just op3' -> return $ Just (Node IFELSE Nothing (Just op1') (Just op2') (Just op3'))
                                    Nothing -> return Nothing
                            else return $ Just (Node IF Nothing (Just op1') (Just op2') Nothing)
                    _ -> return Nothing

            Token While _ _ -> do
                put (idx + 1, tokens)
                op1 <- parenExpr
                op2 <- statement
                case (op1, op2) of
                  (Just op1', Just op2') -> return $ Just (Node WHILE Nothing (Just op1') (Just op2') Nothing)
                  _ -> return Nothing

            Token Lbra _ _ -> do
                put (idx + 1, tokens)
                (node, _) <- parseSeq (Node SEQ Nothing Nothing Nothing Nothing)
                return node
            Token Semicol _ _ -> do
                put (idx + 1, tokens)
                return $ Just emptyNode
            Token Print _ _ -> do
                put (idx + 1, tokens)
                op1 <- parenExpr
                case op1 of
                  Just op1' -> return $ Just (Node PRINT Nothing (Just op1') Nothing Nothing)
                  Nothing -> return Nothing
            _ -> do
                node <- expr
                (idx', tokens') <- get
                if idx' < length tokens' && tokenType (tokens' !! idx') == Semicol
                    then do
                        put (idx' + 1, tokens')
                        return node
                    else return Nothing

parseSeq :: Node -> ParserM (Maybe Node, Bool)
parseSeq currentSeqNode = do
    (idx, tokens) <- get
    if idx < length tokens && tokenType (tokens !! idx) /= Rbra
      then do
        op2 <- statement
        case op2 of
          Just op2' -> do
            (idx', tokens') <- get
            if idx' < length tokens' && tokenType (tokens' !! idx') /= Rbra
              then do
                put (idx', tokens')
                (nextNode, _) <- parseSeq (Node SEQ Nothing (Just currentSeqNode) (Just op2') Nothing)
                return (nextNode, False)
              else do
                put (idx', tokens')
                return (Just $ Node SEQ Nothing (Just currentSeqNode) (Just op2') Nothing, True)
          Nothing -> return (Nothing, False)
      else do
        put (idx + 1, tokens)
        return (Just currentSeqNode, True)

parenExpr :: ParserM (Maybe Node)
parenExpr = do
    (idx, tokens) <- get
    if idx < length tokens && tokenType (tokens !! idx) == Lpar
        then do
            put (idx + 1, tokens)
            node <- expr
            (idx', tokens') <- get
            if idx' < length tokens' && tokenType (tokens' !! idx') == Rpar
                then do
                    put (idx' + 1, tokens')
                    return node
                else return Nothing
        else return Nothing

expr :: ParserM (Maybe Node)
expr = do
    (idx, tokens) <- get
    initialIndex <- gets fst
    leftNode <- test
    (idx', tokens') <- get
    case leftNode of
        Just leftNode'@(Node VAR _ _ _ _) -> if idx' < length tokens' && tokenType (tokens' !! idx') == Assig
            then do
                put (idx' + 1, tokens')
                op2 <- test
                case op2 of
                    Just op2' -> return $ Just (Node SET Nothing (Just leftNode') (Just op2') Nothing)
                    Nothing -> return Nothing
            else do
                put (initialIndex, tokens')
                test
        _ -> test

test :: ParserM (Maybe Node)
test = do
    node <- arExpr
    (idx, tokens) <- get
    case node of
        Just node'@(Node _ _ _ _ _) -> if idx < length tokens && tokenType (tokens !! idx) == Less
            then do
                put (idx + 1, tokens)
                op2 <- arExpr
                case op2 of
                    Just op2' -> return $ Just (Node LESSTHEN Nothing (Just node') (Just op2') Nothing)
                    Nothing -> return Nothing
            else return node
        Nothing -> return Nothing

arExpr :: ParserM (Maybe Node)
arExpr = do
    node <- term
    arExpr' node

arExpr' :: Maybe Node -> ParserM (Maybe Node)
arExpr' node = do
    (idx, tokens) <- get
    case node of
        Just node'@(Node _ _ _ _ _) -> if idx < length tokens && tokenType (tokens !! idx) `elem` [Div, Sub, Add, Mul]
            then do
                let nodeType' = case tokenType (tokens !! idx) of
                                    Div -> DIV
                                    Sub -> SUB
                                    Mul -> MUL
                                    Add -> ADD
                                    _ -> EMPTY
                put (idx + 1, tokens)
                op2 <- term
                case op2 of
                    Just op2' -> arExpr' $ Just (Node nodeType' Nothing (Just node') (Just op2') Nothing)
                    Nothing -> return Nothing
            else return node
        Nothing -> return Nothing

term :: ParserM (Maybe Node)
term = do
    (idx, tokens) <- get
    if idx < length tokens
        then case tokens !! idx of
            Token Var _ (Just var) -> do
                put (idx + 1, tokens)
                return $ Just (Node VAR (Just (fromEnum var - fromEnum 'a')) Nothing Nothing Nothing)
            Token Num (Just val) _ -> do
                put (idx + 1, tokens)
                return $ Just (Node CONST (Just val) Nothing Nothing Nothing)
            Token Lpar _ _ -> parenExpr
            _ -> return Nothing
        else return Nothing


main :: IO ()
main = do
    let input = "var x 42 x print"
        initialState = initCompilerState input
    let (tokens, _) = runState tokenize initialState
    print tokens
    case runParser tokens of
        Just ast -> print ast
        Nothing -> putStrLn "Parsing failed"
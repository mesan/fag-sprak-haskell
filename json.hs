import Data.Char
import System.Environment

data Node = ObjectNode { props :: [(String, Node)] }
          | ArrayNode { items :: [Node] }
          | StringNode String
          | NumberNode Int
          | BoolNode Bool
          | NullNode deriving (Show)

data Token = ObjectStart
           | ObjectEnd
           | ArrayStart
           | ArrayEnd
           | Comma
           | Colon
           | StringLiteral String
           | NumberLiteral Int
           | BoolLiteral Bool
           | Null
           | End deriving (Show)

main = do
    a <- getArgs
    case a of
        ["--verbose", file] -> do
            input <- readFile file
            putStrLn ""
            putStrLn "Input:"
            putStrLn input
            putStrLn ""
            putStrLn "Tokens:"
            print $ tokenize input
            putStrLn ""
            putStrLn "Syntax Tree:"
            print $ parse $ tokenize input
            putStrLn ""
            putStrLn "Pretty:"
            putStrLn $ pretty (parse (tokenize input), "")
            putStrLn ""
        [file] -> do
            input <- readFile file
            putStrLn $ pretty (parse (tokenize input), "")
        _ -> do
            putStrLn "Usage: json [--verbose] <file>"

--------------------------------------------------------------------------------
-- Prettifier
--------------------------------------------------------------------------------

pretty :: (Node, String) -> String
pretty (node, indent) =
    case node of
        ObjectNode props -> "{\n" ++ prettyObject (props, indent ++ "  ") ++ indent ++ "}"
        ArrayNode items -> "[\n" ++ prettyArray (items, indent ++ "  ") ++ indent ++ "]"
        StringNode s -> "\"" ++ s ++ "\""
        NumberNode n -> show n
        BoolNode b -> if b then "true" else "false"
        NullNode -> "null"

prettyArray :: ([Node], String) -> String
prettyArray ([], indent) = ""
prettyArray (node:[], indent) =
    indent ++ pretty (node, indent) ++ "\n"
prettyArray (node:nodes, indent) =
    indent ++ pretty (node, indent) ++ ",\n" ++ prettyArray (nodes, indent)

prettyObject :: ([(String, Node)], String) -> String
prettyObject ([], indent) = ""
prettyObject ((name, node):[], indent) =
    indent ++ "\"" ++ name ++ "\": " ++ pretty (node, indent) ++ "\n"
prettyObject ((name, node):props, indent) =
    indent ++ "\"" ++ name ++ "\": " ++ pretty (node, indent) ++ ",\n" ++ prettyObject (props, indent)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

parse :: [Token] -> Node
parse toks =
    let (node, toks') = parseNode toks
    in
        case lookAhead toks' of
        End -> node
        _ -> error $ "Leftover tokens: " ++ show toks'

parseNode :: [Token] -> (Node, [Token])
parseNode toks =
    case lookAhead toks of
        ObjectStart ->
            let (props, toks') = parseObject (accept toks)
            in
                case lookAhead toks' of
                    ObjectEnd -> (ObjectNode props, accept toks')
                    _ -> error $ "Missing right parenthesis"
        ArrayStart ->
            let (nodes, toks') = parseArray (accept toks)
            in
                case lookAhead toks' of
                    ArrayEnd -> (ArrayNode nodes, accept toks')
                    _ -> error $ "Missing right bracket"
        StringLiteral s -> (StringNode s, accept toks)
        NumberLiteral n -> (NumberNode n, accept toks)
        BoolLiteral b -> (BoolNode b, accept toks)
        Null -> (NullNode, accept toks)
        _ -> error $ "Parse error on token " ++ show toks

parseArray :: [Token] -> ([Node], [Token])
parseArray toks =
    let (node, toks') = parseNode (toks)
    in
        case lookAhead toks' of
        Comma ->
            let (nodes, toks'') = parseArray (accept toks')
            in (node:nodes, toks'')
        ArrayEnd -> ([node], toks')
        _ -> error $ "Parse error on token " ++ show toks'

parseObject :: [Token] -> ([(String, Node)], [Token])
parseObject toks =
    let (ident, toks') = parseIdentifier (toks)
    in
        case lookAhead toks' of
        Colon ->
            let (node, toks'') = parseNode (accept toks')
            in
                case lookAhead toks'' of
                    Comma ->
                        let (props, toks''') = parseObject (accept toks'')
                        in ((ident, node):props, toks''')
                    ObjectEnd -> ([(ident, node)], toks'')
                    _ -> error $ "Parse error on token " ++ show toks''
        _ -> error $ "Parse error on token " ++ show toks'

parseIdentifier :: [Token] -> (String, [Token])
parseIdentifier toks =
    case lookAhead toks of
        StringLiteral s -> (s, accept toks)
        _ -> error $ "Parse error on token " ++ show toks

lookAhead :: [Token] -> Token
lookAhead [] = End
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

--------------------------------------------------------------------------------
-- Tokenizer
--------------------------------------------------------------------------------

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':cs) = tokenize cs
tokenize ('\t':cs) = tokenize cs
tokenize ('\n':cs) = tokenize cs
tokenize ('{':cs) = ObjectStart : tokenize cs
tokenize ('}':cs) = ObjectEnd : tokenize cs
tokenize ('[':cs) = ArrayStart : tokenize cs
tokenize (']':cs) = ArrayEnd : tokenize cs
tokenize (',':cs) = Comma : tokenize cs
tokenize (':':cs) = Colon : tokenize cs
tokenize ('t':'r':'u':'e':cs) = BoolLiteral True : tokenize cs
tokenize ('f':'a':'l':'s':'e':cs) = BoolLiteral False : tokenize cs
tokenize ('n':'u':'l':'l':cs) = Null : tokenize cs
tokenize ('"':cs) = tokenizeString cs
tokenize (c:cs)
    | isDigit c = tokenizeNumber (c:cs)
    | otherwise = error $ "Cannot tokenize " ++ [c]

tokenizeString :: String -> [Token]
tokenizeString s =
    let (str, c:cs) = span (\x -> x /= '"') s in
    StringLiteral str : tokenize cs

tokenizeNumber :: String -> [Token]
tokenizeNumber s =
    let (num, cs) = span isDigit s in
    NumberLiteral (read num) : tokenize cs


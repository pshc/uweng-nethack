import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P

data Level = Level

type DesParser = GenParser Char Level

random ctor = reserved "random" >> return ctor

data RandChar = FixedChar Char | RandChar deriving Show
randChar = (FixedChar `fmap` charLiteral) <|> random RandChar

maze :: Parser (String, RandChar)
maze = do "MAZE" <- whiteSpace >> identifier
          s <- colon >> stringLiteral
          c <- comma >> randChar
          return (s, c)

main = do r <- parseFromFile maze "castle.des"
          case r of Left e  -> putStr "parse error at " >> print e
                    Right x -> print x

--lexer :: TokenParser ()
lexer = P.makeTokenParser (emptyDef { P.commentLine = "#",
                                      P.caseSensitive = False,
                                      P.reservedNames = nms })
  where
    nms = ["random"]

whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
squares = P.squares lexer
comma = P.comma lexer
colon = P.colon lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
decimal = P.decimal lexer
identifier = P.identifier lexer
reserved = P.reserved lexer

{-
mazeLine = ["MAZE", ":", "\"castle-2\"", ",", "' '"]

type DesParser = GenParser String ()

maze :: DesParser (String, Char)
maze = 

main = do either putStrLn print (runParser maze () "" mazeLine)
-}

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:

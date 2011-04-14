
> import Data.Map (Map, (!))
> import Control.Applicative hiding (many, (<|>), optional)
> import qualified Data.Map as Map
> import Text.Parsec
> import Text.Parsec.String
> import Data.Char (toLower)


== Types

A cell id

> data Id = Id Char Int
>      deriving (Show, Eq, Ord)

A value of a cell

> data Value = None | Number Int | Text String | Formula Expression | Error 
>      deriving (Show)

Expression?

> data Operation = Plus | Minus | Mult | Div
>      deriving (Show)

> data Expression = ENumber Int
>                 | EOp Operation Expression Expression
>                 | ELink Id

>      deriving (Show)

> data Document = Document { rows  :: Int,
>                            cols  :: Int,
>                            cells :: Map Id Value }
>      deriving (Show)


== makeDocument

> makeDocument :: Int -> Int -> [[Value]] -> Document
> makeDocument r c vss = Document { rows = r,
>                                   cols = c,
>                                   cells = Map.fromList mapper }
>                where mapper = concat $ zipWith makeRow [1..] vss
>                      makeRow i vs = zipWith (\j v -> (Id j i, v)) ['a'..] vs

== Parser

Parser

> document :: Parser Document
> document = do rows <- number
>               many1 space
>               columns <- number
>               many1 space
>               values <- count rows $ cellRow columns
>               return $ makeDocument rows columns values

> cellRow :: Int -> Parser [Value]
> cellRow n = count n $ do v <- cellValue
>                          many space
>                          return v


> cellValue :: Parser Value
> cellValue = (Formula <$> (char '=' >> expression))
>         <|> (Text <$> (char '\'' >> many alphaNum))
>         <|> (Number <$> number)
>         <|> (return None)

> number :: Parser Int
> number = read <$> many1 digit

Expressions are a bit harder

> operation :: Char -> Operation
> operation '+' = Plus
> operation '-' = Minus
> operation '*' = Mult
> operation '/' = Div

> operator :: Parser Operation
> operator = operation <$> oneOf "+-*/"

> expression :: Parser Expression
> expression = do t <- term
>                 opts <- many (do op <- operator
>                                  next <- term
>                                  return (op, next))
>                 return $ makeExpressionTree t opts


> makeExpressionTree :: Expression -> [(Operation, Expression)] -> Expression
> makeExpressionTree ex [] = ex
> makeExpressionTree ex ((hop, hex):opexs) = makeExpressionTree (EOp hop ex hex) opexs

> term :: Parser Expression
> term = (ENumber <$> number) <|> elink

> elink :: Parser Expression
> elink = do c <- toLower <$> letter
>            m <- (read . pure) <$> digit
>            return (ELink $ Id c m)

== Working with document

> normalize :: Document -> Document
> normalize doc = doc { cells = Map.fromList (zip ids $ map getCell ids) }
>           where cs = cells doc
>                 ids = Map.keys cs
>                 getCell = normalizeCell . (cs !)
>                 normalizeCell (Formula e) = calculate e
>                 normalizeCell c = c
>                 calculate (ELink id) = getCell id
>                 calculate (ENumber n) = Number n
>                 calculate (EOp op a b) = operate op (calculate a) (calculate b)
>                 operate op (Number a) (Number b) = Number $ opr op a b
>                 operate _ _ _ = Main.Error

> opr Plus = (+)
> opr Minus = (-)
> opr Mult = (*)
> opr Div = div


== Program

> main :: IO ()
> main = do c <- getContents
>           let doc' = parse document "Input" c
>           case doc' of
>               Right doc -> print $ normalize doc
>               Left err -> print $ err

module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

parseTermSymbol :: Parser (Exp Int -> Exp Int -> Exp Int)
parseTermSymbol = 
  do { reservedOp lis "+"; return Plus}
  <|> do { reservedOp lis "-"; return Minus} 

parseFactSymbol :: Parser (Exp Int -> Exp Int -> Exp Int)
parseFactSymbol = 
  do { reservedOp lis "*"; return Times }
  <|> do { reservedOp lis "/"; return Div } 

-- parseVar se construye un parser
-- con la clasificacion de palabras clave
parseVar :: Parser (Exp Int)
parseVar = 
  do v <- identifier lis
     return (Var v)

-- Parser de variables incrementadas en 1
parseVarInc :: Parser (Exp Int)
parseVarInc =
  do varName <- identifier lis
     reservedOp lis "++"
     return (VarInc varName) 

-- Parser de variable decrementadas en 1
parseVarDec :: Parser (Exp Int)
parseVarDec =
  do varName <- identifier lis
     reservedOp lis "--"
     return (VarDec varName) 

-- Parser de números enteros negativos
parseUMinus :: Parser (Exp Int)
parseUMinus = 
  do reservedOp lis "-"
     e <- (try parseNat) <|> intexp
     return (UMinus e)

-- Parser de números naturales
parseNat :: Parser (Exp Int)
parseNat = 
  do n <- (natural lis)
     return (Const (fromIntegral n))

atom :: Parser (Exp Int)
atom = try parseVarInc
       <|> try parseVarDec
       <|> try parseUMinus
       <|> try parseVar
       <|> try parseNat
       <|> parens lis intexp

parseTerm :: Parser (Exp Int)
parseTerm = chainl1 atom parseFactSymbol

intexp :: Parser (Exp Int)
intexp = chainl1 parseTerm parseTermSymbol

-- parser utiles
-- braces: parsea las llaves
-- try: si falla un lado del choice, revierte lo leido
-- chainl1: hackage.org/package/parsec-3

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

-- Parser de valores booleanos
parseBoolVal :: Parser (Exp Bool)
parseBoolVal = do { reserved lis "true"; return BTrue }
               <|> do { reserved lis "false"; return BFalse }

-- Parser de operadores de comparación
parseComparisonOp = do { reservedOp lis "==" ; return Eq }
                    <|> do { reservedOp lis "!=" ; return NEq }
                    <|> do { reservedOp lis "<" ; return Lt }
                    <|> do { reservedOp lis ">" ; return Gt }

-- Parser de comparaciones
parseComparision :: Parser (Exp Bool)
parseComparision =
  do x <- intexp
     comp <- parseComparisonOp
     y <- intexp
     return (comp x y)

parseNot :: Parser (Exp Bool)
parseNot = do reservedOp lis "!"
              x <- boolexp
              return (Not x)


parseWithinAnd :: Parser (Exp Bool)
parseWithinAnd = (try parseBoolVal)
                <|> (try parseNot)
                <|> (try parseComparision)
                <|> (parens lis boolexp)

parseWithinOr :: Parser (Exp Bool)
parseWithinOr = chainl1 parseWithinAnd
  (do { reservedOp lis "&&"; return And })

boolexp :: Parser (Exp Bool)
boolexp = chainl1 parseWithinOr
  (do { reservedOp lis "||"; return Or })

-----------------------------------
--- Parser de comandos
-----------------------------------

parseSkip :: Parser Comm
parseSkip =
  do reserved lis "Skip"
     return Skip

parseLet :: Parser Comm
parseLet =
  do varName <- identifier lis
     reservedOp lis "="
     iexp <- intexp
     return (Let varName iexp)

parseIfThenElse :: Parser Comm
parseIfThenElse =
  do reserved lis "if"
     bexp <- boolexp
     reservedOp lis "{"
     com1 <- comm
     reservedOp lis "}"
     reserved lis "else"
     reservedOp lis "{"
     com2 <- comm
     reservedOp lis "}"
     return (IfThenElse bexp com1 com2)

parseIfThen :: Parser Comm
parseIfThen =
  do reserved lis "if"
     bexp <- boolexp
     reservedOp lis "{"
     com1 <- comm
     reservedOp lis "}"
     return (IfThen bexp com1)

parseRep :: Parser Comm
parseRep = 
  do reserved lis "repeat"
     reservedOp lis "{"
     c <- comm
     reservedOp lis "}"
     reserved lis "until"
     cond <- boolexp
     return (RepeatUntil c cond)

comm1 :: Parser Comm
comm1 = (try parseSkip)
      <|> (try parseLet)
      <|> (try parseIfThenElse)
      <|> (try parseIfThen)
      <|> (parseRep)

parseSeq = do { reservedOp lis ";"; return Seq }

comm :: Parser Comm
comm = chainl1 comm1 parseSeq

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
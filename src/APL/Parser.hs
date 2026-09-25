module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "print",
    "get",
    "put"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lSLiteral :: Parser String
lSLiteral = lexeme $ try $ do
  _ <- satisfy (=='\"')
  cs <- many $ satisfy (\c -> c /= '\"')
  _ <- satisfy (=='\"')
  pure cs

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool = choice $
  [ const True <$> lKeyword "true",
    const False <$> lKeyword "false"
  ]

-- Atom ::= Int
--        | Bool
--        | Var
--        | "(" Exp ")"
pAtom :: Parser Exp
pAtom = choice $
  [ CstInt <$> lInteger,
    CstBool <$> pBool,
    Var <$> lVName,
    lString "(" *> pExp <* lString ")"
  ]

-- BFunExp := "print" string Atom
--          | "get" Atom
--          | "set" Atom Atom
pBFunExp :: Parser Exp
pBFunExp = choice $
  [ do
      lKeyword "print"
      s <- lSLiteral
      a <- pAtom
      pure (Print s a),
    do
      lKeyword "get"
      a <- pAtom
      pure (KvGet a),
    do
      lKeyword "put"
      a0 <- pAtom
      a1 <- pAtom
      pure (KvPut a0 a1)
  ]

-- FunExp ::= BFunExp
--          | Atom
--          | FunExp Atom
pFunExp :: Parser Exp
pFunExp = choice [ pBFunExp, pAtom >>= chain ]
  where 
    chain x = choice $
      [
        do
          y <- pAtom
          chain $ Apply x y,
        pure x
      ]

-- CtrlExp ::= "if" FunExp "then" FunExp "else" FunExp
--           | FunExp
pCtrlExp :: Parser Exp
pCtrlExp = choice $
  [ If
      <$> (lKeyword "if" *> pExp)
      <*> (lKeyword "then" *> pExp)
      <*> (lKeyword "else" *> pExp),
    pFunExp
  ]

-- PowExp ::= CtrlExp "**" PowExp
--          | CtrlExp
pPowExp :: Parser Exp
pPowExp = pCtrlExp >>= chain
  where
    chain x = choice $
      [
        do
          lString "**"
          y <- pPowExp 
          chain $ Pow x y,
        pure x
      ]

-- FacExp ::= FacExp "*" PowExp
--          | FacExp "/" PowExp
--          | PowExp
pFacExp :: Parser Exp
pFacExp = pPowExp >>= chain
  where
    chain x = choice $
      [ do
          lString "*"
          y <- pPowExp
          chain $ Mul x y,
        do
          lString "/"
          y <- pPowExp
          chain $ Div x y,
        pure x
      ]

-- TermExp ::= TermExp "+" FacExp
--           | TermExp "-" FacExp
--           | FacExp
pTermExp :: Parser Exp
pTermExp = pFacExp >>= chain
  where
    chain x = choice $
      [ do
          lString "+"
          y <- pFacExp
          chain $ Add x y,
        do
          lString "-"
          y <- pFacExp
          chain $ Sub x y,
        pure x
      ]

-- BoolExp ::= BoolExp "==" TermExp
--           | TermExp
pBoolExp :: Parser Exp
pBoolExp = pTermExp >>= chain
  where
    chain x = choice $
      [
        do
          lString "=="
          y <- pTermExp
          chain $ Eql x y,
        pure x
      ]

-- Exp ::=
--       | BoolExp
pExp :: Parser Exp
pExp = pBoolExp

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

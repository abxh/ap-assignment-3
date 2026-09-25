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
    "false"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

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

-- FunExp ::= FunExp Atom
--          | Atom
pFunExp :: Parser Exp
pFunExp = pAtom >>= chain
  where 
    chain x = choice $
      [ do
          y <- pAtom
          chain $ Apply x y,
        pure x
      ]

-- CtrlExp ::= if Exp then Exp else Exp
--           | FunExp
pCtrlExp :: Parser Exp
pCtrlExp = choice $
  [ If
      <$> (lKeyword "if" *> pExp)
      <*> (lKeyword "then" *> pExp)
      <*> (lKeyword "else" *> pExp),
    pFunExp
  ]

-- FacExp ::= FacExp "*" CtrlExp
--          | FacExp "/" CtrlExp
--          | CtrlExp
pFacExp :: Parser Exp
pFacExp = pCtrlExp >>= chain
  where
    chain x = choice $
      [ do
          lString "*"
          y <- pCtrlExp
          chain $ Mul x y,
        do
          lString "/"
          y <- pCtrlExp
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

-- Exp ::= TermExp
pExp :: Parser Exp
pExp = pTermExp

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

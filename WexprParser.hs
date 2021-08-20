module WexprParser where

import ParserLib
import WexprDef
import Control.Applicative

wexpr :: Parser Wexpr
wexpr = (expr >>= \a -> def >>= \x -> many (andef) >>= \xs -> pure (Where a (x:xs))) <|> op <|> expr 
    where
        equation = identifier ["where", "and"] >>= \v -> operator "=" >> expr >>= \e -> return (v, e)
        andef = keyword "and" >> equation
        def = keyword "where" >> equation

expr :: Parser Wexpr
expr = fmap Nat natural <|> fmap Var (identifier ["where", "and"]) <|> (openParen *> wexpr <* closeParen) <|> neg

pow :: Parser Wexpr
pow = chainr1 expr (operator "^" *> pure (Pow))

mult :: Parser Wexpr
mult = chainl1 pow (operator "*" *> pure (Times))

op :: Parser Wexpr
op = chainl1 mult ((operator "+" *> pure (Plus)) <|> (operator "-" *> pure (Minus)))

neg :: Parser Wexpr
neg = operator "-" >> expr >>= (\a -> pure (Neg a))
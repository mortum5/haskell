module Parse
    ( Prs(Prs,runPrs),
    Parser(Parser,runParser),
    anyChr,
    lowers,
    lower,
    digit,
    char,
    many',
    many1
    ) where

import Control.Applicative
import Data.Char
import Control.Monad

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
newtype Parser a = Parser { runParser :: String -> [(a,String)]}
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Applicative Prs where
  pure a = Prs fun where
    fun s = Just (a,s)
  pf <*> ps = Prs fun where
    fun s = case runPrs pf s of
      Nothing -> Nothing
      (Just (x,s')) -> case runPrs ps s' of
        Nothing -> Nothing
        (Just (y,s'')) -> Just (x y, s'')

instance Alternative Prs where
  empty = Prs $ const Nothing
  pf <|> ps = Prs fun where
    fun s = runPrs pf s <|> runPrs ps s


instance Functor Parser where
  fmap f p = Parser fun where
    fun s = case runParser p s of
      [] -> []
      ((x,s):xs) -> [(f x, s)]

instance Applicative Parser where
  pure a = Parser fun where
    fun s = [(a,s)]
  pf <*> ps = Parser fun where
    fun s = case runParser pf s of
      [] -> []
      ((x,s'):xs) -> case runParser ps s' of
        [] -> []
        ((y, s''):ys) -> [(x y, s'')]

instance Alternative Parser where
  empty = Parser $ const []
  pf <|> ps = Parser fun where
    fun s = case runParser pf s of
      []  -> case runParser ps s of
        [] -> []
        ans -> ans
      ans -> ans


instance Functor PrsE where
  fmap f p = PrsE fun where
      fun s = case runPrsE p s of
          (Left x) -> Left x
          (Right (x, s)) -> Right (f x, s)


instance Functor (Prs) where
  fmap f p = Prs fun where
    fun s = case runPrs p s of
      Nothing -> Nothing
      Just (x, w) -> Just (f x, w)



satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser fun where
  fun "" = []
  fun (x:xs) | p x = [(x,xs)]
             | otherwise = []


satisfy' :: (Char -> Bool) -> Prs Char
satisfy' p = Prs fun where
  fun "" = Nothing
  fun (x:xs) | p x = Just (x,xs)
             | otherwise = Nothing

lowers :: Parser String
lowers = pure (:) <*> lower <*> lowers <|> pure ""

lower :: Parser Char
lower = satisfy isLower

many' :: Prs a -> Prs [a]
many' p = pure (:) <*> p <*> many' p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = pure (:) <*> (p <|> empty) <*> many' p

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

char y = Prs fun where
  fun "" = Nothing
  fun (x:xs) | (x == y) = Just (x,xs)
             | otherwise = Nothing



anyChr :: Parser Char
anyChr = Parser f where
  f "" = []
  f (x:xs) = [(x,xs)]


nat = read <$> many' digit


digit = satisfy' isDigit

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Configured.Parser where

import           Control.Applicative
import           Control.Lens          (each, (%~), _1)
import           Control.Monad
import           Data.Char             (isAlpha, isAlphaNum)
import           Data.Configured.Types
import           Data.Function         (($), (&))
import           Data.Semigroup
import qualified Data.Text             as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta.Parser

name :: Parser Name
name = do
  c <- satisfy isAlpha
  s <- many $ satisfy (\e -> isAlpha e || e == '-' || e == '_')
  pure . T.cons c $ T.pack s

value :: Parser Value
value = vBool <|> vString <|> vNumber <|> vList

vBool :: Parser Value
vBool = fmap Bool $ (const True <$> string "true")
    <|> (const False <$> string "false")
    <|> onOff
  where
    onOff = char 'o' *> ((const True <$> char 'n') <|> (const False <$> string "ff"))

vString :: Parser Value
vString = undefined

vNumber :: Parser Value
vNumber = undefined

listStart :: Parser ()
listStart = void $ char '['

listEnd :: Parser ()
listEnd = void $ char ']'

listSeparator :: Parser ()
listSeparator = void $ char ',' 

delimited :: Show b => Parser a -> Parser b -> Parser [b]
delimited u p = emptyList <|> list
  where
    emptyList = const [] <$> notFollowedBy p
    list      = (:) <$> p <*> many (u *> p)

vList :: Parser Value
vList = between listStart listEnd $ List <$> delimited listSeparator value
  
valueBindChar :: Char
valueBindChar = '='

valueBind :: Name -> Parser (Name, Value)
valueBind n = (n,) <$> (char valueBindChar *> value)

groupStart :: Parser ()
groupStart = void $ char '{'

groupEnd :: Parser ()
groupEnd = void $ char '}'

prefixGroup :: Name -> Name -> Name
prefixGroup g n = (g <> "." <> n)

groupBind :: Name -> Parser [(Name, Value)]
groupBind g = between groupStart groupEnd $ (each . _1 %~ prefixGroup g) . join <$> many boundValueOrGroup

boundValueOrGroup :: Parser [(Name, Value)]
boundValueOrGroup = do
  n <- name
  (pure <$> valueBind n) <|> groupBind n


commentChar :: Char
commentChar = '#'

comment :: Parser ()
comment = char commentChar *> void (manyTill anyChar $ char '\n')

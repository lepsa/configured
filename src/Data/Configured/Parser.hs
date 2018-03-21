{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Configured.Parser where

import           Control.Applicative
import           Control.Lens            (each, (%~), _1)
import           Control.Monad
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.State.Class
import           Data.Bool
import           Data.Char
import           Data.Configured.Types
import           Data.Function           (($), (&))
import           Data.List
import           Data.Semigroup
import qualified Data.Text               as T
import           Prelude                 hiding (String)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Read               (readMaybe)
import           Text.Trifecta.Parser

type ConfigParser m = (Monad m, CharParsing m, TokenParsing m)


parser :: ConfigParser m => m [(Name, Either Import Value)]
parser = 



import' :: ConfigParser m => Name -> m (Name, Import)
import' n = (n,) . Import . T.pack <$> (symbol "import" *> comment *> string' <* comment)

name :: ConfigParser m => m Name
name =
  (T.pack <$> ident >>= \t -> bool err (pure t) (t == "import") <?> "identifier") <* comment
  where
    ident = (:) <$> letter <*> many (letter <|> oneOf "_-")
    err = unexpected "reserved word (import) used as identifier"

value :: ConfigParser m => m Value
value = (vBool <|> vString <|> vNumber <|> vList) <* comment

vBool :: ConfigParser m => m Value
vBool = fmap Bool $ (const True <$> string "true")
    <|> (const False <$> string "false")
    <|> onOff
    <?> "bool"
  where
    onOff :: ConfigParser m => m Bool
    onOff = char 'o' *> ((const True <$> char 'n') <|> (const False <$> string "ff"))

-- Sadly I can't use the stringLiteral token here. Configurator has a different
-- unicode code-point escape scheme.
escape :: ConfigParser m => m ()
escape = void $ char '\\'

escapedChar :: ConfigParser m => m Char
escapedChar = escape *> (newline <|> carriageReturn <|> horizontalTab <|> backslash <|> doubleQuote <|> unicode)
  where
    newline        = const '\n' <$> char 'n'
    carriageReturn = const '\r' <$> char 'r'
    horizontalTab  = const '\t' <$> char 't'
    backslash      = char '\\'
    doubleQuote    = char '"'

unicode :: ConfigParser m => m Char
unicode = do
  h <- hex
  bool (pure $ chr h) (surrogateChr h <$> (escape *> surrogate)) $ isSurrogate h
  where
    isSurrogate a = a >= 0xD800 && a <= 0xDFFF
    surrogateChr h l = chr $ 0x10000 + ((h - 0xd800) * 0x400) + (l - 0xdc00)
    inSet l a = 
      let err = unexpected $ "Unexpected " <> show a <> ", expected one of the following to form a unicode surrogate: " <> intersperse ' ' l
      in  bool err (pure a) $ elem a l

surrogate :: ConfigParser m => m Int
surrogate = do
  _ <- char 'u'
  a <- digitToInt <$> oneOf ['d', 'D']
  b <- digitToInt <$> (satisfyRange '8' '9' <|> satisfyRange 'a' 'f' <|> satisfyRange 'A' 'F')
  c <- digitToInt <$> satisfy isHexDigit
  d <- digitToInt <$> satisfy isHexDigit
  pure $ hexToInt a b c d

hex :: ConfigParser m => m Int
hex = char 'u' *> hexes
  where
    hex'  = digitToInt <$> satisfy isHexDigit
    hexes = hexToInt <$> hex' <*> hex' <*> hex' <*> hex'

hexToInt :: Integral a => a -> a -> a -> a -> a
hexToInt h1 h2 h3 h4 = b16 h1 3 + b16 h2 2 + b16 h3 1 + b16 h4 0
  where
    b16 :: Integral a => a -> a -> a
    b16 i o = i * (16 ^ o)


-- Ensure that we aren't picking up characters that are meant to be escaped.
unescapedChar :: ConfigParser m => m Char
unescapedChar = satisfy $ flip notElem ['\n', '\r', '\t', '\\', '"']

stringChar :: ConfigParser m => m Char
stringChar = escapedChar <|> unescapedChar

stringStart :: ConfigParser m => m ()
stringStart = void $ char '"'

stringEnd :: ConfigParser m => m ()
stringEnd = stringStart

string' :: ConfigParser m => m [Char]
string' = between stringStart stringEnd (many stringChar) <?> "string"

vString :: ConfigParser m => m Value
vString = String . T.pack <$> string'

vNumber :: ConfigParser m => m Value
vNumber = Number . fromInteger <$> integer <?> "number"

vList :: ConfigParser m => m Value
vList = List <$> between start end (sepBy value $ comma <* comment) <* comment
  where
    start = symbolic '[' <* comment
    end   = symbolic ']' <* comment

valueBind :: ConfigParser m => Name -> m (Name, Value)
valueBind n = (n,) <$> (symbolic '=' *> comment *> value)

prefixGroup :: Name -> Name -> Name
prefixGroup g n = (g <> "." <> n)

groupBind :: ConfigParser m => Name -> m [(Name, Value)]
groupBind g = between start end $ (each . _1 %~ prefixGroup g) . join <$> many boundValueOrGroup
  where
    start = symbolic '{' <* comment
    end   = symbolic '}' <* comment

boundValueOrGroup :: ConfigParser m => m [(Name, Value)]
boundValueOrGroup = name >>= \n -> (pure <$> valueBind n) <|> groupBind n

comment :: ConfigParser m => m ()
comment = skipOptional $ many comment'

comment' :: ConfigParser m => m ()
comment' = symbolic '#' *> token (void . manyTill anyChar $ void newline <|> eof)

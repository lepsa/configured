{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Configured.Parser where

import           Control.Applicative
import           Control.Lens            (each, (%~), _1)
import           Control.Monad
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Bool
import           Data.Char
import           Data.Configured.Types
import           Data.Function           (($), (&))
import           Data.List
import           Data.Semigroup
import qualified Data.Text               as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Read               (readMaybe)
import           Text.Trifecta.Parser

type ConfigParser m = (MonadIO m, Monad m, CharParsing m)

import' :: ConfigParser m => m [(Name, Value)]
import' = do
  string "import" *> spaces
  undefined

name :: ConfigParser m => m Name
name = (\c s -> T.cons c $ T.pack s)
  <$> satisfy isAlpha
  <*> many (satisfy (\e -> isAlpha e || e == '-' || e == '_'))

value :: ConfigParser m => m Value
value = vBool <|> vString <|> vNumber <|> vList

vBool :: CharParsing m => m Value
vBool = fmap Bool $ (const True <$> string "true")
    <|> (const False <$> string "false")
    <|> onOff
  where
    onOff :: CharParsing m => m Bool
    onOff = char 'o' *> ((const True <$> char 'n') <|> (const False <$> string "ff"))


escape :: CharParsing m => m ()
escape = void $ char '\\'

escapedChar :: ConfigParser m => m Char
escapedChar = escape *> (newline <|> carriageReturn <|> horizontalTab <|> backslash <|> doubleQuote <|> unicode)
  where
    newline        = const '\n' <$> char 'n'
    carriageReturn = const '\r' <$> char 'r'
    horizontalTab  = const '\t' <$> char 't'
    backslash      = char '\\'
    doubleQuote    = char '"'

isSurrogate :: Int -> Bool
isSurrogate a = a >= 0xD800 && a <= 0xDFFF

surrogateChr :: Int -> Int -> Char
surrogateChr h l = chr $ 0x10000 + ((h - 0xd800) * 0x400) + (l - 0xdc00)

inSet :: ConfigParser m => [Char] -> Char -> m Char
inSet l a = bool err (pure a) $ elem a l
  where
    err = unexpected $ "Unexpected " <> show a <> ", expected one of the following to form a unicode surrogate: " <> intersperse ' ' l

surrogate :: ConfigParser m => m Int
surrogate = do
  _ <- char 'u'
  a <- digitToInt <$> oneOf ['d', 'D']
  b <- digitToInt <$> oneOf (['8', '9'] <> ['a'..'f'] <> ['A'..'F'])
  c <- digitToInt <$> satisfy isHexDigit
  d <- digitToInt <$> satisfy isHexDigit
  pure $ hexToInt a b c d

hex :: CharParsing m => m Int
hex = char 'u' *> hexes
  where
    hex'  = digitToInt <$> satisfy isHexDigit
    hexes = hexToInt <$> hex' <*> hex' <*> hex' <*> hex'

hexToInt :: Integral a => a -> a -> a -> a -> a
hexToInt h1 h2 h3 h4 = b16 h1 3 + b16 h2 2 + b16 h3 1 + b16 h4 0
  where
    b16 :: Integral a => a -> a -> a
    b16 i o = i * (16 ^ o)

unicode :: ConfigParser m => m Char
unicode = do
  h <- hex
  bool
    (pure $ chr h)
    (surrogateChr h <$> (escape *> surrogate))
    $ isSurrogate h

-- Ensure that we aren't picking up characters that are meant to be escaped.
unescapedChar :: CharParsing m => m Char
unescapedChar = satisfy $ flip notElem ['\n', '\r', '\t', '\\', '"']

stringChar :: ConfigParser m => m Char
stringChar = escapedChar <|> unescapedChar

stringStart :: CharParsing m => m ()
stringStart = void $ char '"'

stringEnd :: CharParsing m => m ()
stringEnd = stringStart

vString :: ConfigParser m => m Value
vString = between stringStart stringEnd $ String . T.pack <$> many stringChar

vNumber :: CharParsing m => m Value
vNumber = Number . foldl f 0 <$> some digit
  where
    f z a = z * 10 + (fromIntegral . toInteger $ digitToInt a)

listStart :: CharParsing m => m ()
listStart = void $ char '['

listEnd :: CharParsing m => m ()
listEnd = void $ char ']'

listSeparator :: CharParsing m => m ()
listSeparator = void $ char ','

vList :: ConfigParser m => m Value
vList = between listStart listEnd $ List <$> sepBy value listSeparator

valueBindChar :: Char
valueBindChar = '='

valueBind :: ConfigParser m => Name -> m (Name, Value)
valueBind n = (n,) <$> (char valueBindChar *> value)

groupStart :: CharParsing m => m ()
groupStart = void $ char '{'

groupEnd :: CharParsing m => m ()
groupEnd = void $ char '}'

prefixGroup :: Name -> Name -> Name
prefixGroup g n = (g <> "." <> n)

groupBind :: ConfigParser m => Name -> m [(Name, Value)]
groupBind g = between groupStart groupEnd $ (each . _1 %~ prefixGroup g) . join <$> many boundValueOrGroup

boundValueOrGroup :: ConfigParser m => m [(Name, Value)]
boundValueOrGroup = name >>= \n -> (pure <$> valueBind n) <|> groupBind n

commentChar :: Char
commentChar = '#'

comment :: CharParsing m => m ()
comment = char commentChar *> void (manyTill anyChar $ char '\n')

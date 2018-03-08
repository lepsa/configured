{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Configured.Types where

import           Control.Applicative         ((<*>))
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Exception           (Exception, SomeException)
import           Control.Lens.TH             (makeLenses)
import           Data.Bool                   (Bool, bool, otherwise)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Char                   (Char)
import           Data.Complex                (Complex)
import           Data.Data                   (Data)
import           Data.Eq                     (Eq, (==))
import           Data.Fixed                  (Fixed, HasResolution)
import           Data.Function               (($), (.))
import           Data.Functor                (Functor, fmap, (<$>))
import           Data.Hashable               (Hashable, hashWithSalt)
import           Data.HashMap.Strict         (HashMap)
import           Data.Int                    (Int, Int16, Int32, Int64, Int8)
import           Data.List                   (isSuffixOf)
import           Data.Maybe                  (Maybe (Just, Nothing))
import           Data.Ratio                  (Ratio, Rational, denominator,
                                              numerator, (%))
import           Data.Semigroup              ((<>))
import           Data.String                 (IsString, fromString)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LT
import           Data.Traversable            (traverse)
import           Data.Tuple                  (fst)
import           Data.Typeable               (Typeable)
import           Data.Word                   (Word, Word16, Word32, Word64,
                                              Word8)
import           Foreign.C.Types             (CDouble, CFloat)
import           Prelude                     (Double, Float, Integer, Integral,
                                              Num, RealFloat, String,
                                              fromInteger, fromRational)
import           System.IO                   (FilePath, IO)
import           Text.Show                   (Show, show)

type Name = T.Text

data AutoConfig = AutoConfig { interval :: Int -- Time in seconds to check for updates
                             , onError  :: SomeException -> IO () -- Action on reload or change error.
                             } deriving (Typeable)
instance Show AutoConfig where
  show c = "AutoConfig {interval = " <> show (interval c) <> "}"

data Worth a = Required { worth :: a }
             | Optional { worth :: a }
             deriving (Functor, Typeable, Eq, Show)

data Value = Bool Bool | String T.Text | Number Rational | List [Value]
  deriving (Eq, Data, Show, Typeable)

data Pattern = Exact Name | Prefix Name
  deriving (Eq, Data, Show, Typeable)

instance IsString Pattern where
  fromString s | isSuffixOf ".*" s = Prefix . T.init . T.pack $ s
               | otherwise         = Exact $ T.pack s

instance Hashable Pattern where
    hashWithSalt salt (Exact n)  = hashWithSalt salt n
    hashWithSalt salt (Prefix n) = hashWithSalt salt n

type ChangeHandler = Name -> Maybe Value -> IO ()

data Config = Config
  { _group    :: Name
  , _paths    :: TVar [(Name, Worth FilePath)]
  , _conf     :: TVar (HashMap Name Value)
  , _handlers :: TVar [(Pattern, ChangeHandler)]
  }
makeLenses ''Config

type Settings = TVar SettingsData
data SettingsData = SettingsData
  { paths'    :: [Worth FilePath]
  , conf'     :: HashMap Name Value
  , handlers' :: [(Pattern, ChangeHandler)]
  }

instance (Hashable a) => Hashable (Worth a) where
  hashWithSalt s a = hashWithSalt s (worth a)

instance IsString (Worth FilePath) where
  fromString = Required



-- Exceptions

data ConfigError = ParseError FilePath String
  deriving (Show, Typeable)

instance Exception ConfigError

data KeyError = KeyError Name
  deriving (Show, Typeable)

instance Exception KeyError



-- Typeclasses

convertRational :: (Num a) => Rational -> Maybe a
convertRational r = bool Nothing (Just . fromInteger $ numerator r) $ denominator r == 1

instance Configured Value where
  convert = Just

class Configured a where
  convert :: Value -> Maybe a

instance Configured Bool where
  convert (Bool b) = Just b
  convert _        = Nothing

instance Configured Char where
  convert (String t) = fst <$> T.uncons t
  convert _          = Nothing

instance Configured Double where
  convert (Number r) = Just $ fromRational r
  convert _          = Nothing

instance Configured Float where
  convert (Number r) = Just $ fromRational r
  convert _          = Nothing

instance Configured Int where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Int8 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Int16 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Int32 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Int64 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Integer where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Word where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Word8 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Word16 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Word32 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured Word64 where
  convert (Number r) = convertRational r
  convert _          = Nothing

instance Configured BS.ByteString where
  convert = fmap T.encodeUtf8 . convert

instance Configured LBS.ByteString where
  convert = fmap LT.encodeUtf8 . convert

instance Configured T.Text where
  convert (String s) = Just s
  convert _          = Nothing

instance Configured CFloat where
  convert (Number r) = Just $ fromRational r
  convert _          = Nothing

instance Configured CDouble where
  convert (Number r) = Just $ fromRational r
  convert _          = Nothing

instance Configured LT.Text where
  convert = fmap LT.fromStrict . convert

instance Configured a => Configured [a] where
  convert (List l) = traverse convert l
  convert _        = Nothing

instance Integral a => Configured (Ratio a) where
  convert (Number r) = let n = numerator r
                           d = denominator r
                       in  Just $ (fromInteger n) % (fromInteger d)
  convert _          = Nothing

instance HasResolution a => Configured (Fixed a) where
  convert (Number r) = Just $ fromRational r
  convert _          = Nothing

instance RealFloat a => Configured (Complex a) where
  convert (Number r) = Just $ fromRational r
  convert _          = Nothing

instance (Configured a, Configured b) => Configured (a, b) where
  convert (List (a:b:_)) = (,) <$> convert a <*> convert b
  convert _              = Nothing

instance (Configured a, Configured b, Configured c) => Configured (a, b, c) where
  convert (List (a:b:c:_)) = (,,) <$> convert a <*> convert b <*> convert c
  convert _                = Nothing

instance (Configured a, Configured b, Configured c, Configured d) => Configured (a, b, c, d) where
  convert (List (a:b:c:d:_)) = (,,,) <$> convert a <*> convert b <*> convert c <*> convert d
  convert _                  = Nothing

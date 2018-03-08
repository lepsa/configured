{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Configured where

-- For now exceptions are being thrown all over the shop.
-- When I make the API nicer, one of the first things will be
-- to add error types and MonadExcept

import           Control.Applicative         (liftA2, pure, (<*>))
import           Control.Concurrent          (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO,
                                              readTVar, readTVarIO, swapTVar,
                                              writeTVar)
import           Control.Exception           (catch, throwIO)
import           Control.Lens                (to, view, (%~), (^.))
import           Control.Monad               (forever, join, void, (<=<), (=<<),
                                              (>=>), (>>), (>>=))
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.STM           (STM, atomically)
import           Data.Bool                   (Bool (False), bool, (||))
import           Data.Configured.Types       (AutoConfig (AutoConfig),
                                              ChangeHandler, Config (Config),
                                              Configured, KeyError (KeyError),
                                              Name, Pattern (Exact, Prefix),
                                              Value, Worth (Optional, Required),
                                              conf, convert, group, handlers,
                                              interval, onError, paths)
import           Data.Eq                     ((==))
import           Data.Foldable               (fold, foldr, traverse_)
import           Data.Function               (const, flip, id, ($), (&), (.))
import           Data.Functor                (fmap, (<$>))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.List                   (nub)
import           Data.Maybe                  (Maybe (Just, Nothing), fromMaybe,
                                              maybe)
import           Data.Monoid                 (mempty)
import           Data.Semigroup              (Semigroup, (<>))
import qualified Data.Text                   as T
import           Data.Traversable            (traverse)
import           Prelude                     (undefined)
import           System.IO                   (FilePath, print)
import           System.IO.Unsafe            (unsafePerformIO)

-- TODO: `require` should probably be a MonadError over IO/MonadIO

-- Config loading

autoReload :: MonadIO m => AutoConfig -> [Worth FilePath] -> m (Config, ThreadId)
autoReload ac = autoReloadGroups ac . addRootGroup

autoReloadGroups :: MonadIO m => AutoConfig -> [(Name, Worth FilePath)] -> m (Config, ThreadId)
autoReloadGroups ac l = do
  c <- loadGroups l
  t <- liftIO . forkIO . catch (forever $ threadDelay (interval ac) >> reload c) $ onError ac
  pure (c, t)

autoConfig :: AutoConfig
autoConfig = AutoConfig { interval = 1, onError = const $ pure () }

-- STM says that this is safe to do!
empty :: Config
empty = unsafePerformIO empty'

empty' :: MonadIO m => m Config
empty' = liftIO $ Config mempty <$> newTVarIO mempty <*> newTVarIO mempty <*> newTVarIO mempty


-- Lookup functions
lookup :: (MonadIO m, Configured a) => Config -> Name -> m (Maybe a)
lookup c n = liftIO $ (convert <=< Map.lookup n) <$> getMap c

lookupDefault :: (MonadIO m, Configured a) => a -> Config -> Name -> m a
lookupDefault a c = fmap (fromMaybe a) . lookup c

require :: (MonadIO m, Configured a) => Config -> Name -> m a
require c n = lookup c n >>= liftIO . maybe (throwIO $ KeyError n) pure



-- Config change notification

prefix :: T.Text -> Pattern
prefix = Prefix

exact :: T.Text -> Pattern
exact = Exact

subscribe :: MonadIO m => Config -> Pattern -> ChangeHandler -> m ()
subscribe c p h = atomicallyM . modifyTVar (c ^. handlers) . Map.insertWith (<>) p $ pure h



-- Low level loading
{-
-- Merge the right set of config into the left config
-- Biased to left values. I hope you like STM!
merge :: MonadIO m => Config -> Config -> m Config
merge c = atomicallyM . merge' c

merge' :: Config -> Config -> STM Config
merge' (Config ag ap ac ah) (Config ag bp bc bh) = do
  void $ f nub ap bp >> f id ac bc >> f id ah bh
  pure $ Config _ ap ac ah
  where
    f :: Semigroup a => (a -> a) -> TVar a -> TVar a -> STM a
    f f' a b = liftA2 (<>) (readTVar a) (readTVar b) >>= swapTVar a . f'
-}

readConfig :: MonadIO m => (Name, Worth FilePath) -> m (HashMap Name Value)
readConfig (n, w) = alterNames (Just . (n <>)) <$> case w of
  Optional fp -> undefined fp
  Required fp -> undefined fp

addRootGroup :: [Worth FilePath] -> [(Name, Worth FilePath)]
addRootGroup = fmap (mempty,)

load :: MonadIO m => [Worth FilePath] -> m Config
load = loadGroups . addRootGroup

loadGroups :: MonadIO m => [(Name, Worth FilePath)] -> m Config
loadGroups l = Config mempty
  <$> liftIO (newTVarIO l)
  <*> (traverse readConfig l >>= liftIO . newTVarIO . fold)
  <*> liftIO (newTVarIO mempty)

reload :: MonadIO m => Config -> m ()
reload c = do
  (l, old) <- atomicallyM $ (,) <$> readTVar (c ^. paths) <*> readTVar (c ^. conf)
  new <- fold <$> traverse readConfig l
  atomicallyM $ writeTVar (c ^. paths) l >> writeTVar (c ^. conf) new
  runHandlers c new old

matches :: Name -> Pattern -> Bool
matches n (Exact p)  = p == n
matches n (Prefix p) = fromMaybe False $ (\t -> t == mempty || T.isPrefixOf "." t) <$> T.stripPrefix p n

applyHandlers :: MonadIO m => Name -> Maybe Value -> [ChangeHandler] -> m ()
applyHandlers n v = liftIO . traverse_ (\h -> h n v)

matchingHandlers :: Name -> HashMap Pattern [ChangeHandler] -> [ChangeHandler]
matchingHandlers n = join . Map.elems . Map.filterWithKey (\k -> const $ matches n k)

runHandlers :: MonadIO m => Config -> HashMap Name Value -> HashMap Name Value -> m ()
runHandlers c new old = atomicallyM (readTVar $ c ^. handlers) >>= f
  where
    f :: MonadIO m => HashMap Pattern [ChangeHandler] -> m ()
    f m = traverse_ (\(n, v) -> applyHandlers n v $ matchingHandlers n m) diffList

    keys :: [Name]
    keys = nub $ Map.keys new <> Map.keys old

    diffList :: [(Name, Maybe Value)]
    diffList = foldr (\n l -> maybe l (\v -> (n, v) : l) $ diff n) mempty keys
    
    diff :: Name -> Maybe (Maybe Value)
    diff n = case (Map.lookup n new, Map.lookup n old) of
      (Nothing, Nothing) -> Nothing
      (Just a, Nothing)  -> Just $ Just a
      (Nothing, Just _)  -> Just Nothing
      (Just a, Just b)   -> bool (Just $ Just a) Nothing $ a == b

subconfig :: Name -> Config -> Config
subconfig n c = c & group %~ (<> "." <> n)

addToConfig :: MonadIO m => [Worth FilePath] -> Config -> m ()
addToConfig l = addGroupsToConfig (addRootGroup l)

addGroupsToConfig :: MonadIO m => [(Name, Worth FilePath)] -> Config -> m ()
addGroupsToConfig l c = atomicallyM (flip modifyTVar (<> l) $ c ^. paths) >> reload c



-- Helper functions

atomicallyM :: MonadIO m => STM a -> m a
atomicallyM = liftIO . atomically

display :: MonadIO m => Config -> m ()
display = getMap >=> liftIO . print

-- If f returns Nothing, the value is dropped, else it is inserted with the modification.
alterNames :: (Name -> Maybe Name) -> HashMap Name Value -> HashMap Name Value
alterNames f = Map.foldrWithKey (g . f) mempty
  where
    g :: Maybe Name -> Value -> HashMap Name Value -> HashMap Name Value
    g n v m = maybe m (\k -> Map.insert k v m) n

getMap :: MonadIO m => Config -> m (HashMap Name Value)
getMap c = alterNames (T.stripPrefix $ c ^. group) <$> atomicallyM (readTVar $ c ^. conf)

{-# LANGUAGE RankNTypes, FlexibleContexts, NamedFieldPuns #-}

module PCS where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import System.Directory
import System.FilePath ((</>))
import Token (AppConfig, TokenResp, Vars (..), currentTokenConfig)
import Util(defaultManager)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Maybe
import Network.HTTP.Conduit (Manager)

data PcsConfig = PcsConfig {
  vars :: Vars,
  pcsManager :: Manager
}

type PcsT m a = (MonadIO m, MonadBaseControl IO m) => ReaderT PcsConfig m a

readAppConfig :: IO PcsConfig
readAppConfig = do
  vars <- currentTokenConfig
  manager <- defaultManager
  return $ PcsConfig vars manager

runPcsT :: (MonadIO m, MonadBaseControl IO m) => PcsConfig -> PcsT m a -> m a
runPcsT conf action = runReaderT action conf




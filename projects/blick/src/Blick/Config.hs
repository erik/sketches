{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blick.Config where

import           Control.Monad.Except     (ExceptT, MonadError)
import           Control.Monad.Reader     (MonadIO, MonadReader, ReaderT)
import           Network.Wai.Handler.Warp (Port)
import           Servant                  (ServantErr)
import           System.Environment       (lookupEnv)

newtype AppT m a
  = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Config
  , MonadError ServantErr
  )

type App = AppT IO
type AppM = ReaderT Config


data Config
  = Config
  { configPort    :: Port
  , configHost    :: String
  , configBaseURL :: String
  } deriving Show


configFromEnv :: IO Config
configFromEnv = do
  port <- getEnv "BLICK_PORT" 8080
  host <- getEnv "BLICK_HOST" "localhost"
  baseURL <- getEnv "BLICK_BASE_URL" $ defaultBaseURL host port

  pure Config
    { configPort = port
    , configHost = host
    , configBaseURL = baseURL
    }

  where
    defaultBaseURL h p =
      "http://" ++ h ++ ":" ++ (show p)


getEnv :: Read a => String -> a -> IO a
getEnv key defaultValue = do
  strValue <- lookupEnv key

  -- TODO: Maybe handle error?
  return $
    maybe defaultValue read strValue
